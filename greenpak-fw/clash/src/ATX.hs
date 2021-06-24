{-# language RecordWildCards #-}
module ATX where

import Clash.Prelude hiding (shift)
import Types
import SPICommands

atxControl :: KnownDomain dom => Input dom -> Output dom
atxControl Input{..} = Output { n_ice_reset = 0
                              , ice_power = 0
                              , main_en = 0
                              , n_cs_en = 0
                              , wdog_inhibit = shift
                              , sdo_enable = done
                              }
  where (shift, done) = withClockResetEnable sclk
                                             (unsafeFromHighPolarity $ pure False)
                                             (toEnable $ pure True) $
                          spiCommandDecoder sdi
