{-# LANGUAGE TupleSections #-}
module HolePlugin.RecursiveSearch where

import TcHoleErrors
import TcHoleFitTypes

import TcRnTypes
import TcRnMonad
import TcMType

import Name
import Data.Maybe
import Utils


findFitsRecursively :: Int -> TypedHole -> [HoleFitCandidate] -> TcM [HoleFit]
findFitsRecursively depth hole hfcs =
-- do the work from TcHoleErrors.hs in the plugin, returning the same result (modulo zonking and sorting and recursive hole fits)
    snd <$> tcFilterHoleFits Nothing hole (ctPred . fromJust $ tyHCt hole, []) hfcs

data MyHoleFit =
MyHoleFit { hfId   :: Id       -- ^ The elements id in the TcM
          , hfCand :: HoleFitCandidate  -- ^ The candidate that was checked.
          , hfType :: TcType -- ^ The type of the id, NOT zonked, can't zonk here.
          , hfContains :: [MyHoleFit] -- ^ The recursive hole fits
          }
  | NormalHoleFit HoleFit -- ^ The base case


recursiveFunction :: Int            -- The current depth of the search
              -> Int                -- The maximum depth of the search
              -> Int                -- The `width' of the search (the number of new holes each recursive call may generate)
              -> TcType             -- The type we need to fit
              -> TypedHole          -- The hole we need to fill
              -> [HoleFitCandidate] -- The candidates
              -> TcM [MyHoleFit]      -- The 'completed' hole fits
recursiveFunction c d w hfcs hole tctype
  | c >  d = return []  -- nothing
  | c == d = NormalHoleFit <$> _          -- only direct fits
  | otherwise = liftM2 (++) (recursiveFunction c c w tctype hfcs) $ do      -- direct fits and recursive calls
      ref_tys <- mapM mkRefTy [1..w] tctype
      holefits <- concat <$> mapM (snd . flip (tcFilterHoleFits Nothing hole) hfcs) ref_tys
      return mapM f holefits
        where
          f :: HoleFit -> TcM MyHoleFit
          f (HoleFit hfid hfcand hftype _ hfmatches _) = mapM (MyHoleFit hfid hfcand hftype . recursiveFunction (c+1) d w hfcs hole) hfmatches --TODO check if hfmatches or hfwrap is the one to use

--fFR :: Int -> TypedHole -> [HoleFitCandidate] -> TcM [HoleFit]
--fFR depth hole hfcs = foldr f (return $ replicate depth []) [0..depth-2]
--    where
--        f :: Int -> [[HoleFit]] -> TcM [[HoleFit]]
--        f i hfs = folr (g i) hfs (hfs !! i)
--        g :: Int -> HoleFit -> [[HoleFit]] -> TcM [[HoleFit]] --ofzo
--        g i hf hfs = foldr (h i) hfs (getNewHoles hf (depth - i))
--        h :: Int -> (HoleFit, Int) -> [[HoleFit]] -> [[HoleFit]]
--        h i (hf, j) = insertAt (i+j) hf
--
--        getNewHoles :: Int -> HoleFit -> TcM [(HoleFit, Int)]
--        getNewHoles maxdepth hf = _
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--fFR' :: Int -> TypedHole -> [HoleFitCandidate] -> TcM [HoleFit]
--fFR' depth hole hfcs = map toRaw . fst $ until (null . snd) (uncurry f) (return [], map ((, depth) . fromCand) hfcs)
--    where
--        f :: TcM [MyHoleFit] -> [(MyHoleFit, Int)] -> (TcM [MyHoleFit], [(MyHoleFit, Int)])
--        f hfs ((hf, i):worklist) = let newhf =
--                                   in
--
--
--
--
--
--
--
--type MyHoleFit = [(TcType, String)]
--
--fromCand :: HoleFitCandidate -> TcM MyHoleFit
--fromCand hfc = do
--    name <- tcLookup getName hfc
--    id <- case name of
--        ATcId {tct_id = id} -> Just id
--        AGlobal (AnId id)   -> Just id
--        AGlobal (AConLike (RealDataCon con)) -> Just $ dataConWrapId con
--    return [(idType id, show name)]
--
--
--toRaw :: MyHoleFit -> HoleFit
--toRaw = unwords . map snd
--
--
--
--
--insertAt :: Int -> a -> [[a]]
--insertAt 0 x (ys:yss) = (x : ys) : yss
--insertAt i x (ys:yss) = ys : appendAt i-1 x yss