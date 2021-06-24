module Utils.TH ( tbits ) where

import Prelude
import Types
import Language.Haskell.TH
import Language.Haskell.TH.Quote

parseTBits :: String -> [TBit]
parseTBits = go
  where
    go :: String -> [TBit]
    go "" = []
    go ('0':cs) = T0 : go cs
    go ('1':cs) = T1 : go cs
    go (_:cs) = go cs

toTypeQ :: TBit -> Q Type
toTypeQ T0 = [t| Types.T0 |]
toTypeQ T1 = [t| Types.T1 |]

tbits :: QuasiQuoter
tbits = QuasiQuoter {
    quoteExp = undefined
  , quotePat = undefined
  , quoteDec = undefined
  , quoteType = \s -> do
      bits <- traverse toTypeQ $ parseTBits s
      pure $ foldr (AppT . AppT PromotedConsT) PromotedNilT bits
  }
