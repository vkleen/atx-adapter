{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module Utils.Generic where

import Prelude

import Data.Proxy
import qualified Generics.SOP as SOP

import Clash.Sized.Index

import GHC.TypeNats
import qualified Hedgehog as H
import qualified Hedgehog.Gen as Gen

constrName :: forall a. (SOP.HasDatatypeInfo a, SOP.Generic a) => a -> String
constrName = getName . SOP.unSOP . SOP.from
  where
    names :: SOP.NP (SOP.K SOP.ConstructorName) (SOP.Code a)
    names = SOP.hliftA (SOP.K . SOP.constructorName) $ SOP.constructorInfo $ SOP.datatypeInfo (Proxy @a)

    getName :: SOP.NS f' (SOP.Code a) -> SOP.CollapseTo SOP.NS SOP.ConstructorName
    getName rep = SOP.hcollapse $ SOP.hzipWith const names rep

class BoundedGen a where
  boundedGen :: H.Gen a

instance KnownNat n => BoundedGen (Index n) where
  boundedGen = Gen.enumBounded

gGen :: forall a. (SOP.Generic a, SOP.All2 BoundedGen (SOP.Code a)) => H.Gen a
gGen = SOP.to <$>
  Gen.choice (SOP.hsequence <$> SOP.apInjs_POP (SOP.hcpure (Proxy @BoundedGen) boundedGen))
