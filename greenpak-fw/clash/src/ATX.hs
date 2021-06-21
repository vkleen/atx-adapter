{-# language RecordWildCards #-}
{-# language MultiParamTypeClasses #-}
{-# language QuantifiedConstraints #-}
{-# language ScopedTypeVariables #-}
{-# language FlexibleInstances #-}
{-# language FlexibleContexts #-}
{-# language PolyKinds #-}
{-# language DefaultSignatures #-}
{-# language UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}
{-# OPTIONS_GHC -fplugin=Clash.TopGen #-}
module ATX where

import Clash.Prelude

import Clash.TopGen

import GHC.Generics
import Generics.Kind
import Generics.Kind.TH

data Input dom = Input { n_atx_on :: Signal dom Bit
                       , cs_pgood :: Signal dom Bit
                       , wdog_timeout :: Signal dom Bit
                       , ice_cdone :: Signal dom Bit
                       , sdi :: Signal dom Bit
                       , sclk :: Signal dom Bit
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

class GToClashTuple (f :: LoT k -> Type) where
  type GClashTuple f (x :: LoT k)
  gtoClashTuple :: f x -> GClashTuple f x

instance GToClashTuple U1 where
  type GClashTuple U1 x = ()
  gtoClashTuple _ = ()

instance GToClashTuple (M1 S ('MetaSel ('Just name) unp str dec) (Field t)) where
  type GClashTuple (M1 S ('MetaSel ('Just name) unp str dec) (Field t)) x = name ::: Interpret t x
  gtoClashTuple (M1 (Field x)) = x

instance GToClashTuple f => GToClashTuple (M1 C c f) where
  type GClashTuple (M1 C c f) x = GClashTuple f x
  gtoClashTuple (M1 x) = gtoClashTuple x

instance GToClashTuple f => GToClashTuple (M1 D c f) where
  type GClashTuple (M1 D c f) x = GClashTuple f x
  gtoClashTuple (M1 x) = gtoClashTuple x

instance (GToClashTuple f, GToClashTuple g) => GToClashTuple (f :*: g) where
  type GClashTuple (f :*: g) x = (GClashTuple f x, GClashTuple g x)
  gtoClashTuple (a :*: b) = (gtoClashTuple a, gtoClashTuple b)

class ToClashTuple (f :: Domain -> Type) where
  type ClashTuple f (dom :: Domain) :: Type
  toClashTuple :: forall (dom :: Domain). f dom -> ClashTuple f dom

  default toClashTuple :: forall (dom :: Domain) .
                        ( GenericK f
                        , GToClashTuple (RepK f)
                        , ClashTuple f dom ~ GClashTuple (RepK f) (dom :&&: LoT0)
                        )
                       => f dom -> ClashTuple f dom
  toClashTuple = gtoClashTuple . fromK @_ @f @(dom :&&: LoT0)

instance ToClashTuple Output where
  type ClashTuple Output dom = GClashTuple (RepK Output) (dom :&&: LoT0)
instance ToClashTuple Input where
  type ClashTuple Input dom = GClashTuple (RepK Input) (dom :&&: LoT0)

atxControl :: Input dom -> Output dom
atxControl = undefined

topEntity :: ClashTuple Input System
          -> ClashTuple Output System
topEntity _ = toClashTuple $ Output 1 0 1 1 0 1
{-# ANN topEntity TopGen #-}

-- topEntity :: "n_atx_on" ::: Signal System Bit
--           -> "cs_pgood" ::: Signal System Bit
--           -> "wdog_timeout" ::: Signal System Bit
--           -> "ice_cdone" ::: Signal System Bit
--           -> "sdi" ::: Signal System Bit
--           -> "sclk" ::: Signal System Bit
--           -> ( "n_ice_reset" ::: Signal System Bit
--              , "ice_power" ::: Signal System Bit
--              , "main_en" ::: Signal System Bit
--              , "n_cs_en" ::: Signal System Bit
--              , "wdog_inhibit" ::: Signal System Bit
--              , "sdo_enable" ::: Signal System Bit
--              )
-- topEntity _ _ _ _ _ _ = toClashTuple (Output 1 0 0 1 1 1)

-- $(reify ''ClashTuple >>= traceShowM >> pure [])
-- makeSynthesize 'topEntity
