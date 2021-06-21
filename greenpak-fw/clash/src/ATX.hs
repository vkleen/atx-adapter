module ATX where

import Clash.Explicit.Prelude

import Types

atxControl :: Input (Signal dom) -> Output (Signal dom)
atxControl _ = Output 1 0 1 1 0 1
