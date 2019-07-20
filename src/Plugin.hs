module Plugin (plugin) where

import GhcPlugins

import TcHoleErrors
import TcRnTypes
import TcRnMonad
import TcMType


import RecursiveSearch

plugin :: Plugin
plugin = defaultPlugin { holeFitPlugin = hfp, pluginRecompile = purePlugin }

hfp :: [CommandLineOption] -> Maybe HoleFitPluginR
hfp opts = Just $ HoleFitPluginR init pluginDef stop
    where   init = makeState opts
            pluginDef ref = HoleFitPlugin { candPlugin = getTypedHole ref
                                          , fitPlugin  = makeResult ref }
            stop ref = updTcRef ref $ const $   MyState 0 Nothing Nothing True []


data MyState = MyState { depth  :: Int                    -- How deeply nested the recursive fits go. Default is 3.
                       , width  :: Maybe Int              -- How many recursive holes can be created at each level. When Nothing, the width at each level is equal to the remaining depth. Default is Nothing.
                       , number :: Maybe Int              -- How many suggestions to return, default is Nothing.
                       , prune  :: Bool                   -- Whether `probably useless' suggestions are pruned, default on
                       , candidates :: [HoleFitCandidate] -- The candidates provided by ghc, these are locals ++ syntax ++ globals
                       }

makeState :: [CommandLineOption] -> TcM (TcRef MyState)
makeState [depth, width, number, prune]     = newTcRef $ MyState (read depth) (read width) (read number) (read prune) []
makeState [depth, width, number]            = makeState [depth, width, number, "True"] -- Default concise
makeState [depth, width]                    = makeState [depth, width, "Nothing"]      -- Default unlimited
makeState [depth]                           = makeState [depth, "Nothing"]             -- Default width of Nothing
makeState _                                 = makeState ["3"]                          -- Default depth of 3

getTypedHole :: TcRef MyState -> TypedHole -> [HoleFitCandidate] -> TcM [HoleFitCandidate]
getTypedHole ref th hfcs = do
    updTcRef ref (\s -> MyState (depth s) (width s) (number s) (prune s) hfcs)
    return [] -- Pass an empty list to ghc's hole fit algorithm

makeResult :: TcRef MyState -> TypedHole -> [HoleFit] -> TcM [HoleFit]
makeResult ref tyh [] = do -- The list of found fits should always be empty here
    state <- readTcRef ref
    let d = depth  state
    let w = width  state
    let n = number state
    let p = prune  state
    let hfcs = candidates state
    findFitsRecursively d w n p tyh hfcs -- Run our own hole fit algorithm