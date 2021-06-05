module Commands where

import Clash.Annotations.BitRepresentation
import Clash.Annotations.BitRepresentation.Deriving

data Command = Reset | PowerOn | IceConf | IceForceGood
{-# ANN module (DataReprAnn
                  $(liftQ [t| Command |])
                  2
                  [ ConstrRepr 'Reset        0b11 0b00 []
                  , ConstrRepr 'PowerOn      0b11 0b01 []
                  , ConstrRepr 'IceConf      0b11 0b10 []
                  , ConstrRepr 'IceForceGood 0b11 0b11 []
                  ]) #-}

deriveBitPack [t| Command |]
