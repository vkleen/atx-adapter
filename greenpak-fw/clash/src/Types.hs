{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PolyKinds #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}
module Types where

import Clash.Explicit.Prelude

import Clash.Tuple
import Generics.Kind hiding ((:~:), Nat)
import Generics.Kind.TH

data Input dom = Input { n_atx_on :: Signal dom Bit
                       , cs_pgood :: Signal dom Bit
                       , wdog_timeout :: Signal dom Bit
                       , ice_cdone :: Signal dom Bit
                       , sdi :: Signal dom Bit
                       , sclk :: Clock dom
                       }

data Output dom = Output { n_ice_reset :: Signal dom Bit
                         , ice_power :: Signal dom Bit
                         , main_en :: Signal dom Bit
                         , n_cs_en :: Signal dom Bit
                         , wdog_inhibit :: Signal dom Bit
                         , sdo_enable :: Signal dom Bit
                         }

deriveGenericK ''Input
deriveGenericK ''Output

instance ToClashTuple Output where
  type ClashTuple Output x = GClashTuple (RepK Output) (x :&&: LoT0)
instance ToClashTuple Input where
  type ClashTuple Input x = GClashTuple (RepK Input) (x :&&: LoT0)

data Command = Reset | PowerOn | IceConf
  deriving stock (Generic, Show, Eq)

instance BitPack Command

data TBit = T0
          | T1
  deriving stock (Generic, Show)

class KnownNat (TBitSize a) => TBitPack a where
  type TBitSize a :: Nat
  toV :: Vec (TBitSize a) Bit
  toBV :: BitVector (TBitSize a)
  toBV = v2bv (toV @a)

instance TBitPack T0 where
  type TBitSize T0 = 1
  toV = singleton 0

instance TBitPack T1 where
  type TBitSize T1 = 1
  toV = singleton 1

instance TBitPack '[] where
  type TBitSize '[] = 0
  toV = Nil

instance (TBitPack a, TBitPack as) => TBitPack (a ': as) where
  type TBitSize (a ': as) = TBitSize a + TBitSize as
  toV = toV @a ++ toV @as
