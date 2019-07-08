{-# LANGUAGE TupleSections #-}
module RecursiveSearch where

import TcHoleErrors
import TcHoleFitTypes

import Name


findFitsRecursively :: Int -> TypedHole -> [HoleFitCandidate] -> TcM [HoleFit]
findFitsRecursively depth hole@TyH{holeRCts, holeImps, holeCt} hfcs = 
    snd <$> tcFilterHoleFits Nothing hole (ctPred holeCt, []) hfcs -- do the work from TcHoleErrors in the plugin, returning the same result (modulo zonking and sorting and recursive hole fits).

fFR :: Int -> TypedHole -> [HoleFitCandidate] -> TcM [HoleFit]
fFR depth hole hfcs = foldr f (return $ replicate depth []) [0..depth-2]
    where
        f :: Int -> [[HoleFit]] -> TcM [[HoleFit]]
        f i hfs = folr (g i) hfs (hfs !! i)
        g :: Int -> HoleFit -> [[HoleFit]] -> TcM [[HoleFit]] --ofzo
        g i hf hfs = foldr (h i) hfs (getNewHoles hf (depth - i))
        h :: Int -> (HoleFit, Int) -> [[HoleFit]] -> [[HoleFit]]
        h i (hf, j) = insertAt (i+j) hf

        getNewHoles :: Int -> HoleFit -> TcM [(HoleFit, Int)]
        getNewHoles maxdepth hf = _















fFR' :: Int -> TypedHole -> [HoleFitCandidate] -> TcM [HoleFit]
fFR' depth hole hfcs = map toRaw . fst $ until (null . snd) (uncurry f) (return [], map ((, depth) . fromCand) hfcs)
    where
        f :: TcM [MyHoleFit] -> [(MyHoleFit, Int)] -> (TcM [MyHoleFit], [(MyHoleFit, Int)])
        f hfs ((hf, i):worklist) = let newhf = 
                                   in 







type MyHoleFit = [(TcType, String)]
fromCand :: HoleFitCandidate -> TcM MyHoleFit
fromCand hfc = do
    name <- tcLookup getName hfc
    id <- case name of
        ATcId {tct_id = id} -> Just id
        AGlobal (AnId id)   -> Just id
        AGlobal (AConLike (RealDataCon con)) -> Just $ dataConWrapId con
    return [(idType id, show name)]


toRaw :: MyHoleFit -> HoleFit
toRaw = unwords . map snd




insertAt :: Int -> a -> [[a]]
insertAt 0 x (ys:yss) = (x : ys) : yss
insertAt i x (ys:yss) = ys : appendAt i-1 x yss