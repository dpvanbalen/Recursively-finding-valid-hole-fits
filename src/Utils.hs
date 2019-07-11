module Utils where

-- source: purely functional data structures, Chris Okasaki
data BQueue a = BQ !Int [a] !Int [a]

check :: Int -> [a] -> Int -> [a] -> BQueue a
check lenf fs lenr rs =
    if lenr <= lenf 
    then BQ lenf fs lenr rs 
    else BQ (lenr+lenf) (fs ++ reverse rs) 0 [] 

head :: BQueue a -> a
head (BQ _ []    _ _) = error "empty queue"
head (BQ _ (x:_) _ _) = x

(|>) :: BQueue a -> a -> BQueue a 
(BQ lenf fs lenr rs) |> x = check lenf fs (lenr + 1) (x:rs)

tail :: BQueue a -> BQueue a
tail (BQ lenf (x:fs) lenr rs) = check (lenf-1) fs lenr rs



data Tree a = Leaf a | Node [Tree a]

--unfoldDepthTree :: (Int -> a -> Either b [a]) -> Int -> a -> Tree b
--unfoldDepthTree f d x = case f d x of
--    Left y   -> Leaf y
--    Right zs -> _

--- things copied from TcHoleErrors.hs

--mkRefTy :: Int -> TcM (TcType, [TcTyVar])
--mkRefTy refLvl = (wrapWithVars &&& id) <$> newTyVars
--  where newTyVars = replicateM refLvl $ setLvl <$>
--                        (newOpenTypeKind >>= newFlexiTyVar)
--        setLvl = flip setMetaTyVarTcLevel hole_lvl
--        wrapWithVars vars = mkVisFunTys (map mkTyVarTy vars) hole_ty