module RecursiveSearch where

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
import Control.Arrow ((&&&))
import Data.Maybe
import Data.List
import Utils
import qualified Data.Map.Strict as M

import Debug.Trace



--TODO make 2 versions of hfcs, with or without the 'badifnewhole'.
findFitsRecursively :: Int -> Maybe Int -> Maybe Int -> Bool -> TypedHole -> [HoleFitCandidate] -> TcM [HoleFit]
findFitsRecursively depth width Nothing prune hole hfcs = do
  let tctype = ctPred . fromJust $ tyHCt hole
  let hfcs' = if prune then filter suggpred hfcs else hfcs
  finalState <- bfsFunction depth 0 width hfcs' hole tctype M.empty
  return . map toRaw . map fst . sortOn snd $ getFitsFromState depth finalState tctype

findFitsRecursively depth width (Just limit) prune hole hfcs = map toRaw . take limit . map fst . sortOn snd . snd <$> foldM (\(s,l) i -> if length l >= limit then return (s,l)
          else do
              s' <- nextState i s
              return (s', getFitsFromState i s' tctype)) (firstState,[]) [1..depth]
            where
                hfcs' = if prune then filter suggpred hfcs else hfcs
                tctype = ctPred . fromJust $ tyHCt hole
                firstState = M.empty
                nextState i = bfsFunction i (depth - i) width hfcs' hole tctype

-- A recursive hole fit
data MyHoleFit =
  MyHoleFit {
            hfCand :: HoleFitCandidate  -- ^ The candidate that was checked.
          , hfContains :: [MyHoleFit] -- ^ The recursive hole fits
          }
  | NormalHoleFit HoleFit -- ^ The base case

-- A partial recursive hole fit
data MyPartialHoleFit =
  MyPartialHoleFit {
            hfCand' :: HoleFitCandidate
          , hfContains' :: [TcType]
          }


-- This state stores the result of calling tcFilterHoleFits, which is the most expensive aspect of the search.
type MemoizeState = M.Map String MemoizeStateElem
data MemoizeStateElem = MemoizeStateElem
      Int                 -- Depth for which this element is evaluated
      [MyPartialHoleFit]  -- Partial hole fits for this type

-- findFitsRecursively either calls this only once, expanding the entire tree in one go, or incrementally, in which case each call adds one layer of depth to the tree.
bfsFunction :: Int                  -- Remaining depth
            -> Int                  -- Difference between max depth and depth of this call (for variable width)
            -> Maybe Int            -- Width of the search
            -> [HoleFitCandidate]   -- The candidates
            -> TypedHole            -- The hole
            -> TcType               -- Type of the hole
            -> MemoizeState         -- Memoizes calls to tcFilterHoleFits to speed up execution time
            -> TcM MemoizeState   -- returns the new state
bfsFunction d diff w hfcs hole tcty state = let tyname = tctyToString tcty in case state M.!? tyname of
         Nothing -> if d <= 0 then return state else do
            let width = fromMaybe (d - 1 + diff) w
            ref_tys <- mapM mkRefTy [0..width]
            holefits <- concat <$> mapM ((snd <$>) . flip (tcFilterHoleFits Nothing hole) hfcs) ref_tys
            let mphfs = map f holefits
            let newState = M.insert tyname (MemoizeStateElem d mphfs) state
            foldM (\s mphf -> foldM (flip $ bfsFunction (d-1) diff w hfcs hole) s (hfContains' mphf)) newState mphfs -- go recursive and expand the entire tree by one step
         Just (MemoizeStateElem i mphfs) -> if i >= d then return state
            else do
              let newState = M.insert tyname (MemoizeStateElem d mphfs) state
              foldM (\s mphf -> foldM (flip $ bfsFunction (d-1) diff w hfcs hole) s (hfContains' mphf)) newState mphfs -- go recursive and expand the entire tree by one step
    where
        mkRefTy :: Int -> TcM (TcType, [TcTyVar])
        mkRefTy refLvl = (wrapWithVars &&& id) <$> newTyVars
          where newTyVars = replicateM refLvl $ setLvl <$> (newOpenTypeKind >>= newFlexiTyVar)
                setLvl = flip setMetaTyVarTcLevel (tcTypeLevel tcty)
                wrapWithVars vars = mkVisFunTys (map mkTyVarTy vars) tcty
        f :: HoleFit -> MyPartialHoleFit
        f (HoleFit _ hfcand _ _ _ hfmatches _) = MyPartialHoleFit hfcand hfmatches

-- Assumes that the state is evaluated to the appropriate depth, if not it errors on design. Indexes all results with their length in words, for sorting.
getFitsFromState :: Int
              -> MemoizeState
              -> TcType
              -> [(MyHoleFit, Int)]
getFitsFromState depth state typ = let MemoizeStateElem _ partialFits = state M.! tctyToString typ in
        concatMap (\(MyPartialHoleFit hfcand hfcontains) ->
          if null hfcontains then [(MyHoleFit hfcand [], 1)]
            else if depth == 1 then []
              else map (\xs -> (MyHoleFit hfcand (map fst xs), 1 + (sum $ map snd xs))) . combinations $ map (getFitsFromState (depth-1) state) hfcontains
        ) partialFits


suggpred :: HoleFitCandidate -> Bool
suggpred hfcand = not $ (showSDocUnsafe . ppr $ getName hfcand) `elem` badSuggestions

badSuggestions :: [String] -- The functions, mostly from Data.List, that are not total or have a tendency to pollute the results for other reasons. Some of these we do allow if they fit the hole perfectly.
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
      , "fail"
      ]


toRaw :: MyHoleFit -> HoleFit
toRaw (NormalHoleFit hf) = hf
toRaw hf = RawHoleFit $ toSDoc hf False
  where toSDoc (MyHoleFit hfcand hfcontains) parenthesize = (if parenthesize && (not . null $ hfcontains) then parens else id) $ -- parenthesize only when needed: recursive calls that have arguments
                    foldl (\y x -> y <+> toSDoc x True) (makeName hfcand) hfcontains

-- if the first character is not a letter, it's an operator (or [], which we also filter out). We parenthesize the operators.
makeName :: HoleFitCandidate -> SDoc
makeName hfcand = let normal = ppr $ getName hfcand in
  if head (showSDocUnsafe normal) `elem` "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ[" then normal else parens normal

-- Shows the type in a way that is exactly general enough to work as index for the Map. TcType does not have an Ord or even Eq instance.
tctyToString :: TcType -> String
tctyToString = showSDocUnsafe . ppr
