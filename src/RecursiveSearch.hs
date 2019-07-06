module RecursiveSearch where

import TcHoleErrors
import TcRnTypes

findFitsRecursively :: Int -> TypedHole -> [HoleFitCandidate] -> TcM [HoleFit]
findFitsRecursively depth typedHole hfcs = undefined