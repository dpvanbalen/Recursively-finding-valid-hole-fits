module HolePlugin.Plugin (plugin) where

import GhcPlugins

import TcHoleErrors
import TcRnTypes
import TcRnTypes
import TcRnMonad
import TcMType


import HolePlugin.RecursiveSearch

plugin :: Plugin
plugin = defaultPlugin { holeFitPlugin = hfp, pluginRecompile = purePlugin }

hfp :: [CommandLineOption] -> Maybe HoleFitPluginR
hfp opts = Just $ HoleFitPluginR init pluginDef stop
    where   init = makeState opts
            pluginDef ref = HoleFitPlugin { candPlugin = getTypedHole ref
                                          , fitPlugin  = makeResult ref }
            stop ref = updTcRef ref $ const $   MyState 0 []


data MyState = MyState { depth :: Int
                       , candidates :: [HoleFitCandidate] }

makeState :: [CommandLineOption] -> TcM (TcRef MyState)
makeState [depth] = newTcRef $ MyState (read depth) []
makeState _ = makeState ["3"] -- Default depth of 3

getTypedHole :: TcRef MyState -> TypedHole -> [HoleFitCandidate] -> TcM [HoleFitCandidate]
getTypedHole ref th hfcs = return [] -- Pass an empty list to ghc's hole fit algorithm

makeResult :: TcRef MyState -> TypedHole -> [HoleFit] -> TcM [HoleFit]
makeResult ref holeType [] = do -- The list of found fits should always be empty here
    state <- readTcRef ref
    let d = depth state
    let hfcs = candidates state
    findFitsRecursively d holeType hfcs -- Run our own hole fit algorithm