{-# language ViewPatterns #-}
{-# language TupleSections #-}
{-# language DeriveDataTypeable #-}
module Clash.TopGen (plugin, TopGen(..)) where

import Data.Data hiding (funResultTy, splitTyConApp, TyCon)
import Control.Monad ((>=>))
import Data.Maybe (maybeToList, catMaybes)

import Prelude
import Clash.Annotations.TopEntity
import GhcPlugins
import FamInstEnv
import NameCache

import Debug.Trace

plugin :: Plugin
plugin = defaultPlugin { installCoreToDos = install
                       }

install :: [CommandLineOption] -> [CoreToDo] -> CoreM [CoreToDo]
install _ todo =
  pure (CoreDoPluginPass "Clash.TopGen" pass : todo)

data TopGen = TopGen deriving (Data)

lookupTColon :: ModGuts -> CoreM TyCon
lookupTColon _ = do
  dflags <- getDynFlags
  orig_name_cache <- getOrigNameCache
  let named_types_module:_ = fst <$> lookupModuleInAllPackages dflags (mkModuleName "Clash.NamedTypes")
      Just tcolon_name = lookupOrigNameCache orig_name_cache named_types_module (mkOccName tcName ":::")

  hsc_env <- getHscEnv
  mty_thing <- liftIO $ lookupTypeHscEnv hsc_env tcolon_name
  let Just ty_thing = mty_thing
  pure $ tyThingTyCon ty_thing

pass :: ModGuts -> CoreM ModGuts
pass g = do
  binders <- findTopGen g
  if null binders
    then pure g
    else do
      famInstEnvs <- (mg_fam_inst_env g,) <$> getPackageFamInstEnv
      tcolonTyCon <- lookupTColon g

      let new_anns = catMaybes $ map (mkAnnotation famInstEnvs tcolonTyCon) binders

      pure $ g { mg_anns = mg_anns g ++ new_anns }

  where findTopGen :: ModGuts -> CoreM [CoreBind]
        findTopGen guts = concat <$> mapM (isTopGen guts >=> pure . maybeToList) (mg_binds guts)

        isTopGen :: ModGuts -> CoreBind -> CoreM (Maybe CoreBind)
        isTopGen guts bndr@(NonRec b _) = do
          anns <- annotationsOn guts b :: CoreM [TopGen]
          if null anns
            then pure Nothing
            else pure $ Just bndr
        isTopGen _ _ = pure Nothing

        mkAnnotation :: FamInstEnvs -> TyCon -> CoreBind -> Maybe Annotation
        mkAnnotation famInstEnvs tcolonTyCon b@(coreBindName -> Just n) = do
          let (args, ret) = splitFunTys (coreBinderType b)
              argsNormal = snd . normaliseType famInstEnvs Nominal <$> args
              retNormal = snd $ normaliseType famInstEnvs Nominal ret

              topEntity = traceShowId $
                mkSynthesize b (mkPortProduct tcolonTyCon <$> argsNormal)
                               (mkPortProduct tcolonTyCon retNormal)
          Just Annotation { ann_target = NamedTarget n
                          , ann_value = toSerialized serializeWithData topEntity
                          }
        mkAnnotation _ _ _ = Nothing

extractNameType :: TyCon -> Type -> Maybe Type
extractNameType tcolon (repSplitTyConApp_maybe -> Just (tc, _:_:n:_)) | tc == tcolon = Just n
extractNameType _ _ = Nothing

coreBinderType :: CoreBind -> Type
coreBinderType (NonRec b _) = varType b
coreBinderType _ = undefined

annotationsOn :: Data a => ModGuts -> CoreBndr -> CoreM [a]
annotationsOn guts bndr = do
  anns <- getAnnotations deserializeWithData guts
  pure $ lookupWithDefaultUFM anns [] (varUnique bndr)

mkPortName :: Maybe Type -> PortName
mkPortName (Just (isStrLitTy -> Just n)) = PortName (unpackFS n)
mkPortName _ = PortName ""

coreBindName :: CoreBind -> Maybe Name
coreBindName (NonRec n _) = Just $ varName n
coreBindName _ = Nothing

mkPortProduct :: TyCon -> Type -> PortName
mkPortProduct tcolon (splitTyConApp_maybe -> Just (f, xs))
  | isTupleTyCon f = PortProduct "" (mkPortProduct tcolon <$> xs)

mkPortProduct tcolon i = mkPortName $ extractNameType tcolon i

mkSynthesize :: CoreBind -> [PortName] -> PortName -> TopEntity
mkSynthesize b ins out =
  let on (coreBindName -> Just n) = occNameString . nameOccName $ n
      on _ = ""
  in Synthesize (on b) ins out
