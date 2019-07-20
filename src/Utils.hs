module Utils where

import Data.List

combinations :: [[a]] -> [[a]]
combinations [] = []
combinations ([]:ys) = []
combinations [xs] = map (\x -> [x]) xs
combinations ((x:xs):ys) = map (x:) (combinations ys) ++ combinations (xs:ys)

takeBound :: Maybe Int -> [a] -> [a]
takeBound Nothing  xs  =  xs
takeBound (Just n) xs  =  take n xs