{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module Tests.SPICommands where

import Prelude
import Data.String (fromString)

import Data.Functor.Identity
import Data.Function ((&))
import Data.Foldable (for_)

import Test.Tasty
import Test.Tasty.TH
import Test.Tasty.Hedgehog

import Hedgehog ((===), (/==))
import qualified Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Utils.Generic

import qualified Generics.SOP as SOP
import qualified Data.SOP.NS as SOP

import Clash.Sized.Index
import GHC.TypeNats
import qualified Clash.Class.BitPack as BitPack
import qualified SPICommands
import Data.Proxy

instance SOP.Generic SPICommands.CommandState
instance SOP.HasDatatypeInfo SPICommands.CommandState

anyCommandState :: H.Gen SPICommands.CommandState
anyCommandState = gGen

prop_roundtrip_bitpack_CommandState :: H.Property
prop_roundtrip_bitpack_CommandState = H.property $ do
  s <- H.forAll anyCommandState
  H.classify (fromString $ constrName s) True
  H.tripping s (BitPack.pack @SPICommands.CommandState) (Identity . BitPack.unpack @SPICommands.CommandState)

tests :: TestTree
tests = $(testGroupGenerator)
