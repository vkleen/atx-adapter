{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module Tests.Types where

import Prelude
import Data.String (fromString)

import Data.Type.Equality

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
import Clash.Class.BitPack
import GHC.TypeNats
import qualified Clash.Class.BitPack as BitPack
import qualified Types
import Data.Proxy

instance SOP.Generic Types.Command
instance SOP.HasDatatypeInfo Types.Command

prop_roundtrip_bitpack_Command :: H.Property
prop_roundtrip_bitpack_Command = H.property $ do
  s <- H.forAll (gGen :: H.Gen Types.Command)
  H.classify (fromString $ constrName s) True
  H.tripping s (BitPack.pack @Types.Command) (Identity . BitPack.unpack @Types.Command)

sizeAssertCommand :: BitSize Types.Command :~: 2
sizeAssertCommand = Refl

tests :: TestTree
tests = $(testGroupGenerator)
