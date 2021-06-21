{-# language BlockArguments #-}
{-# language RecordWildCards #-}

module Main where

import Prelude

import Clash.Shake

import Development.Shake
import Development.Shake.FilePath

outDir :: FilePath
outDir = "_build"

main :: IO ()
main = shakeArgs shakeOptions{ shakeFiles = outDir } do
  useConfig "build.mk"

  phony "clean" do
    putNormal $ "Cleaning files in " <> outDir
    removeFilesAfter outDir [ "//*" ]

  ClashKit{..} <- clashRules (outDir </> "clash") Verilog
    [ "src" ]
    "Top"
    [ ] $
    pure ()

  phony "verilog" $ need [outDir </> "clash" </> "Top.topEntity" </> "clash-manifest.json"]
  phony "clashi" $ clash ["--interactive", "src/ATX.hs"]
