{-# LANGUAGE TupleSections, TypeSynonymInstances #-}
module HolePlugin.RecursiveSearch where

import TcHoleErrors
import TcHoleFitTypes

import TcRnTypes
import TcRnMonad
import TcMType
import TcType
import Id
import Outputable

import Name
import Control.Monad
--import Control.Monad.State
import Control.Arrow ((&&&))
import Data.Maybe
import Data.List
import Utils
import qualified Data.Map.Strict as M

import Debug.Trace




findFitsRecursively :: Int -> Int -> Maybe Int -> Bool -> TypedHole -> [HoleFitCandidate] -> TcM [HoleFit]
findFitsRecursively depth width number prune hole hfcs = do
  let tctype = ctPred . fromJust $ tyHCt hole
  finalState <- foldM (\state i -> bfsFunction i width prune hfcs hole tctype state) M.empty [1..depth]
  return . map toRaw . takeBound number $ getFitsFromState depth finalState tctype
--findFitsRecursively depth width number prune hole hfcs = (map toRaw) . takeBound number <$> recursiveFunction 1 depth width prune hfcs hole (ctPred . fromJust $ tyHCt hole)
-- do the work from TcHoleErrors.hs in the plugin, returning the same result (modulo zonking and sorting and recursive hole fits)
--    snd <$> tcFilterHoleFits Nothing hole (ctPred . fromJust $ tyHCt hole, []) hfcs

data MyHoleFit =
  MyHoleFit { --hfId   :: Id       -- ^ The elements id in the TcM
            hfCand :: HoleFitCandidate  -- ^ The candidate that was checked.
--          , hfType :: TcType -- ^ The type of the id, NOT zonked, can't zonk here.
          , hfContains :: [MyHoleFit] -- ^ The recursive hole fits
          }
  | NormalHoleFit HoleFit -- ^ The base case

data MyPartialHoleFit =
  MyPartialHoleFit { --hfId :: Id
            hfCand' :: HoleFitCandidate
--          , hfType :: TcType
          , hfContains' :: [TcType]}
--  | CompletedHoleFit MyHoleFit
--  | TODOHoleFit TcType


data MemoizeStateElem = MemoizeStateElem
      Int                 -- Depth for which this element is evaluated
--      [(Int,MyHoleFit)]   -- Completed hole fits for this type, with their depth, sorted on depth
      [MyPartialHoleFit]  -- Partial hole fits for this type

type MemoizeState = M.Map String MemoizeStateElem

bfsFunction :: Int                  -- Current depth
            -> Int                  -- Width of the search
            -> Bool                 -- Whether we prune `probably irrelevant' suggestions
            -> [HoleFitCandidate]   -- The candidates
            -> TypedHole            -- The hole
            -> TcType               -- Type of the hole
            -> MemoizeState         -- Memoizes calls to tcFilterHoleFits to speed up execution time
            -> TcM (MemoizeState)   -- returns the new state
bfsFunction d w prune hfcs hole tcty state = let tyname = (uglyTctyToString tcty) in case state M.!? tyname of
         Nothing -> trace (show d ++ tyname) $ do
            ref_tys <- mapM mkRefTy [0..w]
            holefits <- concat <$> mapM ((snd <$>) . flip (tcFilterHoleFits Nothing hole) hfcs) ref_tys
            let mphfs = mapMaybe f holefits
            return (M.insert tyname (MemoizeStateElem d mphfs) state)
         Just (MemoizeStateElem i mphfs) -> if i >= d then return state
            else do
              let newState = M.insert tyname (MemoizeStateElem d mphfs) state
              finalState <- foldM (\s mphf -> foldM (\s' typ -> bfsFunction (d-1) w prune hfcs hole typ s') s (hfContains' mphf)) newState mphfs -- go recursive and expand the entire tree by one step
              return finalState --TODO check speed diff M.insert and M.adjust
    where
        mkRefTy :: Int -> TcM (TcType, [TcTyVar])
        mkRefTy refLvl = (wrapWithVars &&& id) <$> newTyVars
          where newTyVars = replicateM refLvl $ setLvl <$> (newOpenTypeKind >>= newFlexiTyVar)
                setLvl = flip setMetaTyVarTcLevel (tcTypeLevel tcty)
                wrapWithVars vars = mkVisFunTys (map mkTyVarTy vars) tcty
        f :: HoleFit -> Maybe MyPartialHoleFit
        f (HoleFit hfid hfcand _ _ _ [] _)
          | prune && (showSDocUnsafe . ppr $ getName hfcand) `elem` badAlways = Nothing
          | otherwise = Just $ MyPartialHoleFit hfcand []
        f (HoleFit hfid hfcand _ _ _ hfmatches _)
          | prune && (showSDocUnsafe . ppr $ getName hfcand) `elem` badSuggestions = Nothing
          | otherwise = Just $ MyPartialHoleFit hfcand hfmatches

-- Assumes that the state is evaluated to the appropriate depth, if not it errors on design
getFitsFromState :: Int
              -> MemoizeState
              -> TcType
              -> [MyHoleFit]
getFitsFromState depth state typ = let MemoizeStateElem _ partialFits = state M.! (uglyTctyToString typ) in
        concatMap (\(MyPartialHoleFit hfcand hfcontains) ->
          if null hfcontains then [MyHoleFit hfcand []]
            else if depth == 1 then []
              else map (MyHoleFit hfcand) . combinations $ map (getFitsFromState (depth-1) state) hfcontains
        ) partialFits



--recursiveFunction :: Int            -- The current depth of the search
--              -> Int                -- The maximum depth of the search
--              -> Int                -- The `width' of the search (the number of new holes each recursive call may generate)
--              -> Bool               -- Whether we prune away `probably irrelevant' suggestions
--              -> [HoleFitCandidate] -- The candidates
--              -> TypedHole          -- The hole we need to fill
--              -> TcType             -- The type we need to fit
--              -> TcM [MyHoleFit]      -- The 'completed' hole fits
--recursiveFunction c d w prune hfcs hole tctype
--  | c >  d = return []  -- nothing
--  --  | c == d = recursiveFunction 0 1 0 hfcs hole tctype -- (map NormalHoleFit) . snd <$> tcFilterHoleFits Nothing hole (tctype, []) hfcs          -- only direct fits
--  | otherwise = do      -- direct fits and recursive calls
--      let width = if c==d then 0 else w
--      ref_tys <- mapM mkRefTy [0..width]
--      holefits <- concat <$> mapM ((snd <$>) . flip (tcFilterHoleFits Nothing hole) hfcs) ref_tys
--      concat <$> mapM f holefits
--        where
--          f :: HoleFit -> TcM [MyHoleFit]
--          f (HoleFit hfid hfcand hftype _ _ [] _)
--            | prune && (showSDocUnsafe . ppr $ getName hfcand) `elem` badAlways = return []
--            | otherwise = return $ [MyHoleFit hfcand hftype []]
--          f (HoleFit hfid hfcand hftype _ _ hfmatches _)
--            | prune && ({-(\x -> trace x x)-} (showSDocUnsafe . ppr $ getName hfcand)) `elem` badSuggestions = return []
--            | otherwise = (map (MyHoleFit hfcand hftype)) . combinations <$> mapM (recursiveFunction (c+1) d w prune hfcs hole) hfmatches --TODO check if hfmatches or hfwrap is the one to use
--          mkRefTy :: Int -> TcM (TcType, [TcTyVar])
--          mkRefTy refLvl = (wrapWithVars &&& id) <$> newTyVars
--            where newTyVars = replicateM refLvl $ setLvl <$>
--                                  (newOpenTypeKind >>= newFlexiTyVar)
--                  setLvl = flip setMetaTyVarTcLevel (tcTypeLevel tctype) -- hole_lvl is not available here? Maybe it's hidden in the tctype
--                  wrapWithVars vars = mkVisFunTys (map mkTyVarTy vars) tctype



badSuggestions :: [String] -- The functions, mostly from Data.List, that are not total or have a tendency to pollute the results for other reasons.
badSuggestions = badIfNewHole ++ badAlways
  where
    badIfNewHole =
      [ "id"
      , "head"
      , "last"
      , "tail"
      , "init"
      , "foldl1"
      , "foldl'"
      , "foldl1'"
      , "foldr1"
      , "maximum"
      , "minimum"
      , "maximumBy"
      , "minimumBy"
      , "!!"
      , "$"
      , "read"
      , "unsafePerformIO"
      , "asTypeOf"
      ]
badAlways :: [String]
badAlways =
  [ "otherwise" -- is True
  , "return"    -- is pure
  , "<$>"
  , "seq"
  , "const"     -- pure is const
  , "minBound"
  , "maxBound"
  , "pi"
  , "$!"
  , "negate"
  , "succ"
  , "fromEnum"
  , "toEnum"
  , "abs"
  , "signum"
  , "subtract"
  , "div"
  , "pred"
  , "mod"
  , "gcd"
  , "lcm"
  , "quot"
  , "rem"
  , "max"
  , "min"
  , "fromIntegral"
  ]


toRaw :: MyHoleFit -> HoleFit
toRaw (NormalHoleFit hf) = hf
toRaw hf = RawHoleFit $ toSDoc hf False
  where toSDoc (MyHoleFit hfcand hfcontains) parenthesize = (if parenthesize && (not . null $ hfcontains) then parens else id) $ -- parenthesize only when needed: recursive calls that have arguments
                    foldl (\y x -> y <+> (toSDoc x True)) (ppr (getName hfcand)) hfcontains




uglyTctyToString :: TcType -> String
uglyTctyToString = (showSDocUnsafe . ppr)
