{-# language BlockArguments #-}
{-# language RecordWildCards #-}
{-# language OverloadedStrings #-}

module Main where

import Prelude

import Clash.Shake
import Greenpak

import Development.Shake
import Development.Shake.FilePath

import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TLB

import Control.Monad (forM_)

import Formatting
import Formatting.ShortFormatters

outDir :: FilePath
outDir = "_build"

normalF :: Format (Action ()) a -> a
normalF f = runFormat f (putNormal . TL.unpack . TLB.toLazyText)

main :: IO ()
main = shakeArgs shakeOptions{ shakeFiles = outDir, shakeLint = Just LintFSATrace } do
  phony "clean" do
    normalF ("Cleaning files in "%s%"\n") outDir
    removeFilesAfter outDir [ "//*" ]

  kit@ClashKit{..} <- clashRules @(TopEntityLoc "Top" "topEntity")
    (outDir </> "clash") Verilog
    [ "src" ]
    [ ] $
    pure ()

  netlist <- greenpakCircuit kit outDir "atx-adapter"
  phony "greenpak/netlist" $ do
    need [netlist]
    normalF ("Produced GreenPak netlist at "%s%"\n") netlist

  phony "greenpak/print-netlist" $ do
    need [netlist]
    putNormal =<< liftIO (readFile netlist)

  phony "verilog" $ do
    need [manifestFile]
    normalF ("Produced Clash manifest at "%s%"\n") manifestFile
