module HolePlugin where

import GhcPlugins

import TcHoleErrors
import TcRnTypes

import RecursiveSearch

plugin :: Plugin
plugin = defaultPlugin { holeFitPlugin = hfp, pluginRecompile = purePlugin }

hfp :: [CommandLineOption] -> Maybe HoleFitPluginR
hfp opts = Just $ HoleFitPluginR init pluginDef stop
    where   init = makeState opts
            pluginDef ref = HoleFitPlugin { candPlugin = getTypedHole ref
                                          , fitPlugin  = makeResult ref }
            stop = const $ return ()

data MyState = MyState { depth :: Int
                       , hole :: Maybe TypedHole
                       , candidates :: [HoleFitCandidate] }

makeState :: [CommandLineOption] -> TcM (TcRef MyState)
makeState [depth] = newTcRef $ MyState (read depth) Nothing []
makeState _ = makeState ["3"] -- default depth of 3

getTypedHole :: TcRef MyState -> TypedHole -> [HoleFitCandidate] -> TcM [HoleFitCandidate]
getTypedHole ref th hfcs = do
            state <- readTcRef ref
            updTcRef $ ref MyState (depth state) (Just th) hfcs
            return [] -- pass an empty list to ghc's hole fit algorithm, since we will compute it all in findFitsRecursively

makeResult :: TcRef MyState -> [HoleFit] -> TcM [HoleFit]
makeResult ref _ = do
    state <- readTcRef ref
    let d = depth state
    let Just holeType = hole state -- Should never be Nothing here
    let hfcs = candidates state
    findFitsRecursively depth holeType hfcs