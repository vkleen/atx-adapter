#!/usr/bin/env nix-shell
#!nix-shell -i runghc --packages "ghc.withPackages (x: [ x.turtle x.aeson x.lens-aeson x.generic-lens ])"

{-# language OverloadedStrings #-}
{-# language OverloadedLabels #-}
{-# language DeriveGeneric #-}
{-# language ScopedTypeVariables #-}
{-# language RecordWildCards #-}
{-# language DuplicateRecordFields #-}
{-# language NamedFieldPuns #-}
{-# language ViewPatterns #-}

import Turtle
import Filesystem (readFile)
import GHC.Generics
import Prelude hiding (FilePath, readFile)

import Data.Aeson
import Data.ByteString.Lazy (fromStrict)

import qualified Control.Lens as L
import Control.Lens.Operators
import Data.Aeson.Lens
import Data.Generics.Labels

import Data.Monoid
import Data.Foldable

import Debug.Trace

import qualified Data.List as LL

data ExternalSignal = ExternalSignal { name :: Text
                                     , number :: Integer
                                     }
  deriving (Generic, Show)

data Cell = LUT4 { config :: Text, ins :: [Integer], out :: Integer }
          | LUT3 { config :: Text, ins :: [Integer], out :: Integer }
          | LUT2 { config :: Text, ins :: [Integer], out :: Integer }
          | DFF { clk :: Integer, d :: Integer, q :: Integer }
          | IBUF { input :: Integer, output :: Integer }
          | OBUF { input :: Integer, output :: Integer }
  deriving (Generic, Show)

data Net = Net { name :: Text, bits :: [Integer] }
  deriving (Generic, Show)

parseCell :: Value -> Maybe Cell
parseCell x = do
  t <- x ^? key "type" . _String
  -- traceShow t (Just ())
  case t of
    "GP_4LUT" -> do
      config <- x ^? key "parameters" . key "INIT" . _String
      in0 <- x ^? key "connections" . key "IN0" . nth 0 . _Integral
      in1 <- x ^? key "connections" . key "IN1" . nth 0 . _Integral
      in2 <- x ^? key "connections" . key "IN2" . nth 0 . _Integral
      in3 <- x ^? key "connections" . key "IN3" . nth 0 . _Integral
      let ins = [ in0, in1, in2, in3 ]
      out <- x ^? key "connections" . key "OUT" . nth 0 . _Integral
      pure LUT4{..}
    "GP_3LUT" -> do
      config <- x ^? key "parameters" . key "INIT" . _String
      in0 <- x ^? key "connections" . key "IN0" . nth 0 . _Integral
      in1 <- x ^? key "connections" . key "IN1" . nth 0 . _Integral
      in2 <- x ^? key "connections" . key "IN2" . nth 0 . _Integral
      let ins = [ in0, in1, in2 ]
      out <- x ^? key "connections" . key "OUT" . nth 0 . _Integral
      pure LUT3{..}
    "GP_2LUT" -> do
      config <- x ^? key "parameters" . key "INIT" . _String
      in0 <- x ^? key "connections" . key "IN0" . nth 0 . _Integral
      in1 <- x ^? key "connections" . key "IN1" . nth 0 . _Integral
      let ins = [ in0, in1 ]
      out <- x ^? key "connections" . key "OUT" . nth 0 . _Integral
      pure LUT2{..}
    "GP_DFF" -> do
      clk <- x ^? key "connections" . key "CLK" . nth 0 . _Integral
      d <- x ^? key "connections" . key "D" . nth 0 . _Integral
      q <- x ^? key "connections" . key "Q" . nth 0 . _Integral
      pure DFF{..}
    "GP_IBUF" -> do
      input <- x ^? key "connections" . key "IN" . nth 0 . _Integral
      output <- x ^? key "connections" . key "OUT" . nth 0 . _Integral
      pure IBUF{..}
    "GP_OBUF" -> do
      input <- x ^? key "connections" . key "IN" . nth 0 . _Integral
      output <- x ^? key "connections" . key "OUT" . nth 0 . _Integral
      pure OBUF{..}
    otherwise -> Nothing

parseNet :: Text -> Value -> Maybe Net
parseNet name x = do
  hide <- x ^? key "hide_name" . _Integral
  guard $ hide == 0
  let bits = x ^.. key "bits" . _Array . L.each . _Integral
  pure Net{..}

findNetName :: [Net] -> [ExternalSignal] -> Integer -> Maybe (Text, Integer)
findNetName ns ps b =
  (do n <- LL.find (\x -> b `elem` x ^. #bits) ns
      ix <- fromIntegral <$> LL.elemIndex b (n ^. #bits)
      pure (n ^. #name, ix)
  ) <|>
  (do n <- LL.find (\x -> b == x ^. #number) ps
      pure (n ^. #name, 0)
  )

adjustExternals :: [Cell] -> [ExternalSignal] -> [ExternalSignal]
adjustExternals cells = appEndo (foldMap (Endo . adjustWith) cells)

  where adjustWith :: Cell -> [ExternalSignal] -> [ExternalSignal]
        adjustWith IBUF{input, output} = L.mapped . L.filtered (\x -> input == x ^. #number) . #number .~ output
        adjustWith OBUF{input, output} = L.mapped . L.filtered (\x -> output == x ^. #number) . #number .~ input
        adjustWith _ = id

cellName :: Cell -> Text
cellName LUT4{} = "GP_4LUT"
cellName LUT3{} = "GP_3LUT"
cellName LUT2{} = "GP_2LUT"
cellName DFF{} = "GP_DFF"
cellName IBUF{} = "GP_IBUF"
cellName OBUF{} = "GP_OBUF"

sortCells :: [Cell] -> [Cell]
sortCells = LL.sortBy comp
  where comp OBUF{} OBUF{} = EQ
        comp OBUF{} _ = LT

        comp IBUF{} IBUF{} = EQ
        comp IBUF{} OBUF{} = LT
        comp IBUF{} _ = LT

        comp DFF{} DFF{} = EQ
        comp DFF{} IBUF{} = GT
        comp DFF{} OBUF{} = GT
        comp DFF{} _ = LT

        comp LUT2{} LUT2{} = EQ
        comp LUT2{} IBUF{} = GT
        comp LUT2{} OBUF{} = GT
        comp LUT2{} DFF{} = GT
        comp LUT2{} _ = LT

        comp LUT3{} LUT3{} = EQ
        comp LUT3{} IBUF{} = GT
        comp LUT3{} OBUF{} = GT
        comp LUT3{} DFF{} = GT
        comp LUT3{} LUT2{} = GT
        comp LUT3{} _ = LT

        comp LUT4{} LUT4{} = EQ
        comp LUT4{} IBUF{} = GT
        comp LUT4{} OBUF{} = GT
        comp LUT4{} DFF{} = GT
        comp LUT4{} LUT2{} = GT
        comp LUT4{} LUT3{} = GT

formatNet :: [Net] -> [ExternalSignal] -> Integer -> Text
formatNet ns ps b = case findNetName ns ps b of
  Just (n, ix) -> if isSingleton b
                  then n
                  else format (s%"["%Turtle.d%"]") n ix
  Nothing -> format Turtle.d b

  where isSingleton :: Integer -> Bool
        isSingleton b = maybe True (== 1) $ (LL.find (\x -> b `elem` x ^. #bits) ns) ^? #_Just . #bits . L.to length

net :: [Net] -> [ExternalSignal] -> Format r (Integer -> r)
net ns ps = makeFormat (formatNet ns ps)

lutParams :: Cell -> Maybe (Text, [Integer], Integer)
lutParams LUT2{..} = Just (config, ins, out)
lutParams LUT3{..} = Just (config, ins, out)
lutParams LUT4{..} = Just (config, ins, out)
lutParams _ = Nothing

formatCell :: [Net] -> [ExternalSignal] -> Cell -> Maybe Text
formatCell ns ps IBUF{} = Nothing
formatCell ns ps OBUF{} = Nothing
formatCell ns ps DFF{clk, d, q} = pure $ format
  ("GP_DFF clk="%net ns ps%" d="%net ns ps%" q="%net ns ps)
  clk d q
formatCell ns ps c@(lutParams -> Just (config, ins, out)) = pure $ format
  (s%" cfg="%s%" "%s%" OUT="%net ns ps)
  (cellName c) config inputs out
  where
    inputs :: Text
    inputs = mconcat . LL.intersperse " " $ L.iconcatMap (\i b -> [format ("IN"%Turtle.d%"="%net ns ps) i b]) ins
formatCell _ _ _ = Nothing

main :: IO ()
main = do
  stdout (inproc "yosys" ["-q", "atx-control.ys"] "")
  Just (yosysOutput :: Value) <- decode . fromStrict <$> readFile "atx-control.json"

  let cells = sortCells $
                L.concatMapOf (key "modules" . key "atx_control" . key "cells" . members)
                              (\x -> parseCell x ^.. #_Just)
                              yosysOutput
      nets = L.iconcatMapOf (key "modules" . key "atx_control" . key "netnames" . members)
                            (\n x -> parseNet n x ^.. #_Just)
                            yosysOutput
      ports = adjustExternals cells $
                L.iconcatMapOf (key "modules" . key "atx_control" . key "ports" . members)
                               (\n x -> [ ExternalSignal n (x ^?! key "bits" . nth 0 . _Integral) ])
                               yosysOutput

  for_ cells $ \c ->
    maybe (pure ()) (echo . unsafeTextToLine) $ formatCell nets ports c
