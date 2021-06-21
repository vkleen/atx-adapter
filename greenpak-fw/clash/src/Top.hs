{-# OPTIONS_GHC -fplugin=Clash.TopGen #-}
module Top where

import Clash.Explicit.Prelude

import Clash.TopGen
import Clash.Tuple

import Types
import ATX

topEntity :: ClashTuple Input (Signal System)
          -> ClashTuple Output (Signal System)
topEntity = toClashTuple . atxControl . fromClashTuple
{-# ANN topEntity TopGen #-}
