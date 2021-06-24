{-# language OverloadedStrings #-}
{-# language OverloadedLabels #-}
{-# language DeriveGeneric #-}
{-# language ScopedTypeVariables #-}
{-# language RecordWildCards #-}
{-# language DuplicateRecordFields #-}
{-# language NamedFieldPuns #-}
{-# language ViewPatterns #-}

module Greenpak where

import Clash.Shake

import Text.Mustache
import qualified Text.Mustache.Compile.TH as TH

import Formatting
import Formatting.ShortFormatters

import Development.Shake
import Development.Shake.Command
import Development.Shake.FilePath

import GHC.Stack (HasCallStack)
import GHC.Generics
import Prelude

import Data.Scientific (toBoundedInteger)

import Data.Maybe (fromMaybe)

import qualified Data.ByteString.Lazy as BL
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TLB

import Data.Aeson
import Data.ByteString.Lazy (fromStrict)

import qualified Control.Lens as L
import Control.Lens.Operators hiding ((.=), (<.>))
import Data.Aeson.Lens
import Data.Generics.Labels

import Control.Applicative ((<|>))
import Control.Monad (guard)

import Data.Monoid
import Data.Foldable

import qualified Data.List as LL

data ExternalSignal = ExternalSignal { name :: TL.Text
                                     , number :: Integer
                                     }
  deriving (Generic, Show, Eq, Ord)

newtype Constant = Constant TL.Text
  deriving (Generic, Show, Eq, Ord)

data Cell = LUT4 { config :: TL.Text, ins :: [Integer], out :: Integer }
          | LUT3 { config :: TL.Text, ins :: [Integer], out :: Integer }
          | LUT2 { config :: TL.Text, ins :: [Integer], out :: Integer }
          | DFF { clk :: Integer, d :: Integer, q :: Integer }
          | DFFSR { srmode :: TL.Text, clk :: Integer, d :: Integer, nsr :: Integer, q :: Integer }
          | IBUF { input :: Integer, output :: Integer }
          | OBUF { input_or_const :: Either Constant Integer, output :: Integer }
  deriving (Generic, Show, Eq, Ord)

data Net = Net { name :: TL.Text, bits :: [Integer] }
  deriving (Generic, Show, Eq, Ord)

parseCell :: Value -> Maybe Cell
parseCell x = do
  t <- x ^? key "type" . _String
  case t of
    "GP_4LUT" -> do
      config <- x ^? key "parameters" . key "INIT" . _String . L.to TL.fromStrict
      in0 <- x ^? key "connections" . key "IN0" . nth 0 . _Integral
      in1 <- x ^? key "connections" . key "IN1" . nth 0 . _Integral
      in2 <- x ^? key "connections" . key "IN2" . nth 0 . _Integral
      in3 <- x ^? key "connections" . key "IN3" . nth 0 . _Integral
      let ins = [ in0, in1, in2, in3 ]
      out <- x ^? key "connections" . key "OUT" . nth 0 . _Integral
      pure LUT4{..}
    "GP_3LUT" -> do
      config <- x ^? key "parameters" . key "INIT" . _String . L.to TL.fromStrict
      in0 <- x ^? key "connections" . key "IN0" . nth 0 . _Integral
      in1 <- x ^? key "connections" . key "IN1" . nth 0 . _Integral
      in2 <- x ^? key "connections" . key "IN2" . nth 0 . _Integral
      let ins = [ in0, in1, in2 ]
      out <- x ^? key "connections" . key "OUT" . nth 0 . _Integral
      pure LUT3{..}
    "GP_2LUT" -> do
      config <- x ^? key "parameters" . key "INIT" . _String . L.to TL.fromStrict
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
    "GP_DFFSR" -> do
      srmode <- x ^? key "parameters" . key "SRMODE" . _String . L.to TL.fromStrict
      clk <- x ^? key "connections" . key "CLK" . nth 0 . _Integral
      d <- x ^? key "connections" . key "D" . nth 0 . _Integral
      nsr <- x ^? key "connections" . key "nSR" . nth 0 . _Integral
      q <- x ^? key "connections" . key "Q" . nth 0 . _Integral
      pure DFFSR{..}
    "GP_IBUF" -> do
      input <- x ^? key "connections" . key "IN" . nth 0 . _Integral
      output <- x ^? key "connections" . key "OUT" . nth 0 . _Integral
      pure IBUF{..}
    "GP_OBUF" -> do
      inputValue <- x ^? key "connections" . key "IN" . nth 0
      input_or_const <- case inputValue of
        Number (toBoundedInteger -> Just (x :: Int)) -> pure $ Right (fromIntegral x)
        String c -> pure . Left . Constant . TL.fromStrict $ c
        _ -> Nothing
      output <- x ^? key "connections" . key "OUT" . nth 0 . _Integral
      pure OBUF{..}
    _ -> Nothing

parseNet :: TL.Text -> Value -> Maybe Net
parseNet name x = do
  hide <- x ^? key "hide_name" . _Integral
  guard $ hide == 0
  let bits = x ^.. key "bits" . _Array . L.each . _Integral
  pure Net{..}

findNetName :: [Net] -> [ExternalSignal] -> Integer -> Maybe (TL.Text, Integer)
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
        adjustWith OBUF{input_or_const = Right x, output} = L.mapped . L.filtered (\x -> output == x ^. #number) . #number .~ x
        adjustWith _ = id

cellName :: Cell -> TL.Text
cellName LUT4{} = "GP_4LUT"
cellName LUT3{} = "GP_3LUT"
cellName LUT2{} = "GP_2LUT"
cellName DFF{} = "GP_DFF"
cellName DFFSR{} = "GP_DFFSR"
cellName IBUF{} = "GP_IBUF"
cellName OBUF{} = "GP_OBUF"

sortCells :: [Cell] -> [Cell]
sortCells = LL.sort

formatNet :: [Net] -> [ExternalSignal] -> Integer -> TLB.Builder
formatNet ns ps b = case findNetName ns ps b of
  Just (n, ix) -> if isSingleton b
                  then TLB.fromLazyText n
                  else bprint (t%"["%int%"]") n ix
  Nothing -> bprint int b

  where isSingleton :: Integer -> Bool
        isSingleton b = maybe True (== 1) $ LL.find (\x -> b `elem` x ^. #bits) ns
                                            ^? #_Just . #bits . L.to length

net :: [Net] -> [ExternalSignal] -> Format r (Integer -> r)
net ns ps = later (formatNet ns ps)

lutParams :: Cell -> Maybe (TL.Text, [Integer], Integer)
lutParams LUT2{..} = Just (config, ins, out)
lutParams LUT3{..} = Just (config, ins, out)
lutParams LUT4{..} = Just (config, ins, out)
lutParams _ = Nothing

formatCell :: [Net] -> [ExternalSignal] -> Cell -> Maybe TLB.Builder
formatCell ns ps IBUF{} = Nothing
formatCell ns ps OBUF{input_or_const = Left (Constant c), output} = pure $ bprint
  ("GP_OBUF const="%t%" out="%net ns ps)
  c output
formatCell ns ps OBUF{} = Nothing
formatCell ns ps DFF{clk, d, q} = pure $ bprint
  ("GP_DFF clk="%net ns ps%" d="%net ns ps%" q="%net ns ps)
  clk d q
formatCell ns ps DFFSR{clk, d, nsr, q} = pure $ bprint
  ("GP_DFFSR clk="%net ns ps%" d="%net ns ps%" nsr="%net ns ps%" q="%net ns ps)
  clk d nsr q
formatCell ns ps c@(lutParams -> Just (config, ins, out)) = pure $ bprint
  (t%" cfg="%t%" "%builder%" OUT="%net ns ps)
  (cellName c) config inputs out
  where
    inputs :: TLB.Builder
    inputs = mconcat . LL.intersperse " " $ L.iconcatMap (\i b -> [bprint ("IN"%int%"="%net ns ps) i b]) ins
formatCell _ _ _ = Nothing

extractCircuit :: Value -> TL.Text
extractCircuit yosysOutput =
  let cells = sortCells $
                L.concatMapOf (key "modules" . key "topEntity" . key "cells" . members)
                              (\x -> parseCell x ^.. #_Just)
                              yosysOutput
      nets = L.iconcatMapOf (key "modules" . key "topEntity" . key "netnames" . members)
                            (\(TL.fromStrict -> n) x -> parseNet n x ^.. #_Just)
                            yosysOutput
      ports = adjustExternals cells $
                L.iconcatMapOf (key "modules" . key "topEntity" . key "ports" . members)
                               (\(TL.fromStrict -> n) x -> [ ExternalSignal n (x ^?! key "bits" . nth 0 . _Integral) ])
                               yosysOutput

      circuit = mconcat $
           (ports <&> \p -> bprint (t%" = "%net nets ports%"\n") (p ^. #name) (p ^. #number))
        <> ["\n"]
        <> [line <> "\n" | Just line <- cells <&> formatCell nets ports]

  in TLB.toLazyText circuit

yosysTemplate :: Template
yosysTemplate = [TH.mustache|
  {{#src_files}}
  read -vlog2k {{file}}
  {{/src_files}}
  synth_greenpak4 -top {{top_entity}} -run :map_luts
  nlutmap -assert -luts {{lut_limits}}
  clean
  synth_greenpak4 -top {{top_entity}} -run map_cells:
  write_json {{json_output}}
|]

greenpakCircuit :: ClashKit -> FilePath -> FilePath -> Rules FilePath
greenpakCircuit ClashKit{..} outDir projectName = let
    jsonOutput = outDir </> projectName <.> "json"
    yosysConfig = outDir </> projectName <.> "ys"
    netlist = outDir </> projectName <.> "net"

    yosys :: (HasCallStack, CmdArguments args) => args :-> Result r
    yosys = cmd ["yosys" :: String]
  in do yosysConfig %> \out -> do
          srcs <- manifestSrcs
          let values = object . mconcat $
                         [ [ "top_entity" .= topEntity ]
                         , [ "lut_limits" .= ("2,8,16,2" :: String) ]
                         , [ "json_output" .= jsonOutput ]
                         , [ "src_files" .= mconcat
                              [ [ object [ "file" .= src ] | src <- srcs ]
                              ]
                           ]
                         ]
          writeFileChanged out . TL.unpack $ renderMustache yosysTemplate values

        jsonOutput %> \out -> do
          need [ yosysConfig ]
          yosys (FileStdout $ outDir </> projectName <.> "yosys_out") [ yosysConfig ]

        netlist %> \out -> do
          need [ jsonOutput ]
          Just (yosysOutput :: Value) <- decode <$> liftIO (BL.readFile jsonOutput)
          writeFileChanged out . TL.unpack $ extractCircuit yosysOutput

        pure netlist
