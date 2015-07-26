{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Supply (SupplyT, evalSupplyT, runSupplyT)
import Control.Monad.Supply.Class (fresh)
import Control.Monad.Error.Class (MonadError)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader (ReaderT, runReaderT)
import Control.Monad (when)
import Control.Monad.Trans.Writer (WriterT, runWriterT)
import qualified Language.PureScript.Constants as C
import Control.Monad.Trans.Except (ExceptT, runExceptT)
import System.Exit (exitSuccess, exitFailure)
import Control.Monad.Reader.Class (MonadReader)
import Control.Monad.Writer.Class (MonadWriter)
import System.IO (hPutStrLn, stderr)
import Language.PureScript.Errors (MultipleErrors)
import Language.PureScript.AST.Declarations (Module ( Module ))
import Language.PureScript.CodeGen.Externs (moduleToPs)
import Language.PureScript.Environment (Environment, initEnvironment)
import Language.PureScript.Linter.Exhaustive (checkExhaustiveModule)
import Language.PureScript.Pretty.JS (prettyPrintJS)
import Language.PureScript.Renamer (renameInModules)
import Language.PureScript.Sugar (desugar)
import Language.PureScript.Sugar.BindingGroups (createBindingGroups, collapseBindingGroups)
import Language.PureScript.Names (runModuleName, ModuleName( ModuleName ), ProperName (ProperName))
import Language.PureScript.TypeChecker (typeCheckModule)
import Language.PureScript.TypeChecker.Monad (runCheck')
import qualified Language.PureScript.CodeGen.JS as J
import qualified Language.PureScript.CoreFn as CF
import Language.PureScript.Options (Options, defaultOptions)
import qualified Language.PureScript as P
import Language.PureScript.AST.Declarations (Declaration ( ImportDeclaration ))
import Language.PureScript.AST.Declarations (ImportDeclarationType( Implicit ), Declaration ( PositionedDeclaration))
import Language.PureScript.Linter (lint)


-- |
-- A monad for running compile actions
--
newtype Compile a = Compile { unCompile :: ReaderT Options (WriterT MultipleErrors (ExceptT MultipleErrors IO)) a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadError MultipleErrors, MonadWriter MultipleErrors, MonadReader Options)

-- |
-- Execute a 'Compile' monad, returning either errors, or the result of the compile plus any warnings.
--
runCompile :: Options -> Compile a -> IO (Either MultipleErrors (a, MultipleErrors))
runCompile opts = runExceptT . runWriterT . flip runReaderT opts . unCompile

--build :: forall m. (Functor m, Applicative m, Monad m, MonadReader Options m, MonadError MultipleErrors m, MonadWriter MultipleErrors m)
--     => Module -> m Environment
build :: Module -> Compile ()
build m = do
    lint m
--    (desugared, nextVar) <- runSupplyT 0 $ zip (map fst (error "TODO1")) <$> desugar (map snd (error "TODO2"))
--    evalSupplyT nextVar $ go initEnvironment desugared
    (desugared, nextVar) <- runSupplyT 0 $ desugar [importPrim m]
    evalSupplyT nextVar $ goT initEnvironment (head desugared)

goT env m@(Module coms moduleName' _ exps) = do
{-
    lift $ progress $ show env
    lift $ progress "Performing TypeCheck..."
    (_, env') <- lift . runCheck' env $ typeCheckModule Nothing m
    lift $ progress "Done"
    lift $ progress $ show env'
    return env'
-}
    lift $ progress $ "Compiling " ++ runModuleName moduleName'
    (checked@(Module _ _ elaborated _), env') <- lift . runCheck' env $ typeCheckModule Nothing m
    lift $ progress $ "-- 1 -- " ++ runModuleName moduleName'
    checkExhaustiveModule env' checked
    lift $ progress $ "-- 2 -- " ++ runModuleName moduleName'
    regrouped <- createBindingGroups moduleName' . collapseBindingGroups $ elaborated
    let mod' = Module coms moduleName' regrouped exps
        corefn = CF.moduleToCoreFn env' mod'
        [renamed] = renameInModules [corefn]
        exts = moduleToPs mod' env'
    nextVar <- fresh
    lift $ codegen renamed env' nextVar exts

progress :: String -> Compile ()
progress = liftIO . putStrLn

--go :: Environment -> [(Bool, Module)] -> SupplyT m Environment
go env [] = return env
go env ((False, m) : ms') = do
    (_, env') <- lift . runCheck' env $ typeCheckModule Nothing m
    go env' ms'
go env ((True, m@(Module coms moduleName' _ exps)) : ms') = do
--    lift $ progress $ "Compiling " ++ runModuleName moduleName'
    (checked@(Module _ _ elaborated _), env') <- lift . runCheck' env $ typeCheckModule Nothing m
    checkExhaustiveModule env' checked
    regrouped <- createBindingGroups moduleName' . collapseBindingGroups $ elaborated
    let mod' = Module coms moduleName' regrouped exps
        corefn = CF.moduleToCoreFn env' mod'
        [renamed] = renameInModules [corefn]
        exts = moduleToPs mod' env'
    nextVar <- fresh
    lift $ codegen renamed env' nextVar exts
    go env' ms'

-- |
-- A value to be used in the Supply monad.
--
type SupplyVar = Integer

-- |
-- Generated code for an externs file.
--
type Externs = String

codegen :: CF.Module CF.Ann -> Environment -> SupplyVar -> Externs -> Compile ()
codegen m _ nextVar exts = do
    let mn = CF.moduleName m
{-
    foreignInclude <- case mn `M.lookup` foreigns of
      Just (path, _)
        | not $ requiresForeign m -> do
            tell $ errorMessage $ UnnecessaryFFIModule mn path
            return Nothing
        | otherwise -> return $ Just $ J.JSApp (J.JSVar "require") [J.JSStringLiteral "./foreign"]
      Nothing | requiresForeign m -> throwError . errorMessage $ MissingFFIModule mn
              | otherwise -> return Nothing
-}
    pjs <- evalSupplyT nextVar $ prettyPrintJS <$> J.moduleToJs m Nothing
    let filePath = runModuleName mn
--        jsFile = outputDir </> filePath </> "index.js"
--        externsFile = outputDir </> filePath </> "externs.purs"
--        foreignFile = outputDir </> filePath </> "foreign.js"
        prefix = ["Generated by psc version " ++ "XXX"]
        js = unlines $ map ("// " ++) prefix ++ [pjs]
    progress "----- js ------"
    progress js
    progress "----- exts ------"
    progress exts
    progress "-----------------"
--    writeTextFile jsFile js
--    maybe (return ()) (writeTextFile foreignFile . snd) $ mn `M.lookup` foreigns
--    writeTextFile externsFile exts


main :: IO ()
main = do
    let opts = defaultOptions

    case runWriterT (P.parseModulesFromFiles id [("Foo.purs", "module Foo (foo) where\nfoo::Int\nfoo=3\n")]) of
        Left errs -> do
          hPutStrLn stderr (P.prettyPrintMultipleErrors (P.optionsVerboseErrors opts) errs)
          exitFailure
        Right (ms, warnings) -> do
          when (P.nonEmpty warnings) $
            hPutStrLn stderr (P.prettyPrintMultipleWarnings (P.optionsVerboseErrors opts) warnings)
          print ms
          --let _ = map requiresForeign (map snd ms)
          --print $ map requiresForeign ms

          --e <- runCompile opts build
          e <- runCompile opts (build (snd (head ms)))
          case e of
              Left errs -> do
                hPutStrLn stderr (P.prettyPrintMultipleErrors (P.optionsVerboseErrors opts) errs)
                exitFailure
              Right (_, warnings') -> do
                when (P.nonEmpty warnings') $
                  putStrLn (P.prettyPrintMultipleWarnings (P.optionsVerboseErrors opts) warnings')
                exitSuccess

--codegen :: CF.Module CF.Ann -> Environment -> SupplyVar -> Externs -> Make ()
--codegen m _ nextVar exts = do error "TODO3"
{-
    let mn = CF.moduleName m
    foreignInclude <- case mn `M.lookup` foreigns of
      Just (path, _)
        | not $ requiresForeign m -> do
            tell $ errorMessage $ UnnecessaryFFIModule mn path
            return Nothing
        | otherwise -> return $ Just $ J.JSApp (J.JSVar "require") [J.JSStringLiteral "./foreign"]
      Nothing | requiresForeign m -> throwError . errorMessage $ MissingFFIModule mn
              | otherwise -> return Nothing
    pjs <- evalSupplyT nextVar $ prettyPrintJS <$> J.moduleToJs m foreignInclude
    let filePath = runModuleName mn
        jsFile = outputDir </> filePath </> "index.js"
        externsFile = outputDir </> filePath </> "externs.purs"
        foreignFile = outputDir </> filePath </> "foreign.js"
        prefix = ["Generated by psc version " ++ "XXX" | usePrefix]
        js = unlines $ map ("// " ++) prefix ++ [pjs]
    writeTextFile jsFile js
    maybe (return ()) (writeTextFile foreignFile . snd) $ mn `M.lookup` foreigns
    writeTextFile externsFile exts
-}


{-
compileMod = do
    let mod' = Module coms moduleName' regrouped exps
        corefn = CF.moduleToCoreFn env' mod'
        [renamed] = renameInModules [error "TODO4"]
    lift $ compile renamed
    return ()

--compile :: CF.Module CF.Ann -> IO ()
compile m = do
    pjs <- evalSupplyT (error "TODO5") $ prettyPrintJS <$> J.moduleToJs m (error "TODO6")
    let _ = undefined
    liftIO $ putStrLn "hi"
    return ()
-}


-- TODO This was copied from Language.PureScript.Make
-- Should be refactored into a common module

-- |
-- Add an import declaration for a module if it does not already explicitly import it.
--
addDefaultImport :: ModuleName -> Module -> Module
addDefaultImport toImport m@(Module coms mn decls exps)  =
  if isExistingImport `any` decls || mn == toImport then m
  else Module coms mn (ImportDeclaration toImport Implicit Nothing : decls) exps
  where
  isExistingImport (ImportDeclaration mn' _ _) | mn' == toImport = True
  isExistingImport (PositionedDeclaration _ _ d) = isExistingImport d
  isExistingImport _ = False

importPrim :: Module -> Module
importPrim = addDefaultImport (ModuleName [ProperName C.prim])
