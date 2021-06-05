module ATX where

import Clash.Prelude

import TopGen

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

topEntity :: "n_atx_on" ::: Signal System Bit
    -> "cs_pgood" ::: Signal System Bit
    -> "wdog_timeout" ::: Signal System Bit
    -> "ice_cdone" ::: Signal System Bit
    -> "sdi" ::: Signal System Bit
    -> "sclk" ::: Signal System Bit
    -> "out" ::: Output System
    -- -> ( "n_ice_reset" ::: Signal System Bit
    --    , "ice_power" ::: Signal System Bit
    --    , "main_en" ::: Signal System Bit
    --    , "n_cs_en" ::: Signal System Bit
    --    , "wdog_inhibit" ::: Signal System Bit
    --    , "sdo_enable" ::: Signal System Bit
    --    )
topEntity _ _ _ _ _ _ = Output 1 0 0 1 1 1
makeSynthesize 'topEntity
