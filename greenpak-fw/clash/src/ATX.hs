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
import Clash.Tuple
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

instance ToClashTuple Output where
  type ClashTuple Output dom = GClashTuple (RepK Output) (dom :&&: LoT0)
instance ToClashTuple Input where
  type ClashTuple Input dom = GClashTuple (RepK Input) (dom :&&: LoT0)

atxControl :: Input dom -> Output dom
atxControl _ = Output 1 0 1 1 0 1

topEntity :: ClashTuple Input System
          -> ClashTuple Output System
topEntity = toClashTuple . atxControl . fromClashTuple
{-# ANN topEntity TopGen #-}
