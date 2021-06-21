{-# language UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}
module Types where

import Clash.Explicit.Prelude

import Clash.Tuple
import Generics.Kind
import Generics.Kind.TH

data Input f = Input { n_atx_on :: f Bit
                     , cs_pgood :: f Bit
                     , wdog_timeout :: f Bit
                     , ice_cdone :: f Bit
                     , sdi :: f Bit
                     , sclk :: f Bit
                     }

data Output f = Output { n_ice_reset :: f Bit
                       , ice_power :: f Bit
                       , main_en :: f Bit
                       , n_cs_en :: f Bit
                       , wdog_inhibit :: f Bit
                       , sdo_enable :: f Bit
                       }

deriveGenericK ''Input
deriveGenericK ''Output

instance ToClashTuple Output where
  type ClashTuple Output x = GClashTuple (RepK Output) (x :&&: LoT0)
instance ToClashTuple Input where
  type ClashTuple Input x = GClashTuple (RepK Input) (x :&&: LoT0)
