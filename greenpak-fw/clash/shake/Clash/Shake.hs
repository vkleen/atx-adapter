{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
module Clash.Shake
    ( HDL(..)
    , ClashKit(..)
    , clashRules
    , TopEntityLoc
    ) where

import Prelude

import Development.Shake
import Development.Shake.FilePath
import Development.Shake.Config
import Development.Shake.Util (parseMakefile)

import qualified Clash.Main as Clash

import Data.List.Split

import qualified Data.Text as T
import qualified Data.HashMap.Strict as HM
import Data.Char (isUpper, toLower)
import Control.Monad (forM_)
import qualified Data.ByteString as BS
import qualified System.Directory as Dir
import Control.Exception (bracket)
import Data.Maybe (fromJust)

import Clash.Driver.Manifest
import Clash.Prelude (pack)

import Data.Kind
import GHC.TypeLits
import GHC.Generics
import Data.Hashable
import Data.Binary
import Control.DeepSeq
import Data.Proxy

data HDL
    = VHDL
    | Verilog
    | SystemVerilog
    deriving stock (Eq, Enum, Bounded, Show, Read)

hdlDir :: HDL -> FilePath
hdlDir VHDL = "vhdl"
hdlDir Verilog = "verilog"
hdlDir SystemVerilog = "systemverilog"

hdlExt :: HDL -> FilePath
hdlExt VHDL = "vhdl"
hdlExt Verilog = "v"
hdlExt SystemVerilog = "sv"

data ClashKit = ClashKit
    { clash :: [String] -> Action ()
    , topEntity :: String
    , manifest :: Action Manifest
    , manifestSrcs :: Action [FilePath]
    , manifestFile :: FilePath
    }

withWorkingDirectory :: FilePath -> IO a -> IO a
withWorkingDirectory dir act =
    bracket Dir.getCurrentDirectory Dir.setCurrentDirectory $ \_ ->
        Dir.setCurrentDirectory dir >> act

type TopEntityLoc :: Symbol -> Symbol -> Type
data TopEntityLoc mod top = TopEntityLoc
  deriving stock (Show, Eq, Generic)
instance Hashable (TopEntityLoc mod top)
instance Binary (TopEntityLoc mod top)
instance NFData (TopEntityLoc mod top)

type instance RuleResult (TopEntityLoc mod top) = [FilePath]

type Fst :: (k, l) -> k
type family Fst x where
  Fst '(a, _) = a

type Snd :: (k, l) -> k
type family Snd x where
  Snd '(_, b) = b

type TopEntityLocT :: Type -> Type
type family TopEntityLocT a where
  TopEntityLocT a = TopEntityLoc (Fst (GetModTop a)) (Snd (GetModTop a))

class (KnownSymbol (Fst (GetModTop a)), KnownSymbol (Snd (GetModTop a))) => KnownTopEntityLoc a where
  type GetModTop a :: (Symbol, Symbol)
  topEntityLoc :: TopEntityLocT a
  topEntityLocCts :: (FilePath, String)

instance (KnownSymbol mod, KnownSymbol top) => KnownTopEntityLoc (TopEntityLoc mod top) where
  type GetModTop (TopEntityLoc mod top) = '(mod, top)
  topEntityLoc = TopEntityLoc
  topEntityLocCts = (symbolVal $ Proxy @mod, symbolVal $ Proxy @top)

clashRules :: forall (topEntityLoc :: Type). KnownTopEntityLoc topEntityLoc
           => FilePath -> HDL -> [FilePath] -> [String] -> Action () -> Rules ClashKit
clashRules outDir hdl srcDirs clashFlags extraGenerated = do
    let clash args = liftIO $ do
            let srcFlags = ["-i" <> srcDir | srcDir <- srcDirs]
            let args' = ["-outputdir", outDir] <> clashFlags <> srcFlags <> args
            Clash.defaultMain args'

    -- TODO: ideally, Clash should pure the manifest, or at least its file location...
    let (src, topEntity) = topEntityLocCts @topEntityLoc
        synModule
          | isModuleName src = src
          | otherwise = "Main"

        synOut = outDir </> synModule <.> topEntity
        manifestFile = synOut </> "clash-manifest.json"
        manifest = do
            need [manifestFile]
            Just manifest <- liftIO $ readManifest manifestFile
            pure manifest

    let manifestSrcs = do
            Manifest{..} <- manifest
            let clashSrcs = map T.unpack componentNames <>
                            [ map toLower topEntity <> "_types" | hdl == VHDL ]
            pure [ synOut </> c <.> hdlExt hdl | c <- clashSrcs ]

    getSrcs <- do
        let depFile = outDir </> "ghc-deps.make"
        depFile %> \out -> do
            alwaysRerun
            clash ["-M", "-dep-suffix", "", "-dep-makefile", out, src]

        addOracle $ \(_ :: TopEntityLocT topEntityLoc) -> do
          need [depFile]
          deps <- parseMakefile <$> liftIO (readFile depFile)
          let isHsSource fn
                | ext `elem` [".hi"] = False
                | ext `elem` [".hs", ".lhs"] = True
                | otherwise = error $ "Unrecognized source file: " <> fn
                where
                  ext = takeExtension fn
              hsDeps = [fn | (_, fns) <- deps, fn <- fns, isHsSource fn]
          pure hsDeps

    manifestFile %> \_out -> do
        need =<< getSrcs (topEntityLoc @topEntityLoc)
        extraGenerated
        clash [case hdl of { VHDL -> "--vhdl"; Verilog -> "--verilog"; SystemVerilog -> "--systemverilog" }, src]

    pure ClashKit{..}

isModuleName :: String -> Bool
isModuleName = all (isUpper . head) . splitOn "."
