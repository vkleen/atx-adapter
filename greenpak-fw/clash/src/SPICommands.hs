{-# LANGUAGE StandaloneKindSignatures #-}
{-# OPTIONS_GHC -fconstraint-solver-iterations=128 #-}
module SPICommands where

import Clash.Prelude
import Clash.Annotations.BitRepresentation
import Clash.Annotations.BitRepresentation.Deriving

import Control.Monad.State

import Data.Singletons.Prelude.List

import Types
import Utils.TH

type CommandPreamble :: [ TBit ]
type CommandPreamble = [tbits| 11010 |]

data CommandState = Idle
                  | Preamble (Index (Length CommandPreamble))
                  | Cmd (Index (BitSize Command))
  deriving stock (Generic, Eq, Show)
{-# ANN module (DataReprAnn
                 $(liftQ [t| CommandState |])
                 3
                 [ ConstrRepr 'Idle 0b111 0b101 []
                 , ConstrRepr 'Cmd 0b110 0b110 [0b001]
                 , ConstrRepr 'Preamble 0b000 0b000 [0b111]
                 ]) #-}

instance NFDataX CommandState

deriveBitPack [t| CommandState |]

generateCommand :: Command -> Vec (Length CommandPreamble + BitSize Command) Bit
generateCommand cmd = toV @CommandPreamble ++ bv2v (pack cmd)

commandRecogniser :: Bit -> State CommandState (Maybe Command)
commandRecogniser _ = pure $ Just Reset

spiCommandDecoder :: ( HiddenClockResetEnable dom )
                  =>   "sdi" ::: Signal dom Bit
                  -> ( "shift" ::: Signal dom Bit
                     , "done" ::: Signal dom Bit
                     )
spiCommandDecoder = unbundle . moore transition output Idle
  where
    transition Idle 0 = Idle
    transition Idle 1 = Cmd 1
    transition (Cmd 1) 0 = Cmd 1
    transition (Cmd 1) 1 = Idle
    output Idle = (0, 0)
    output (Cmd 1) = (1, 1)
