-----------------------------------------------------------------------------
--
-- Module      :  Language.PureScript.Compile
-- Copyright   :  (c) PureScript 2015
-- License     :  MIT
--
-- Maintainer  :  Phil Freeman <paf31@cantab.net>
-- Stability   :  experimental
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Language.PureScript.Compile
  ( getModuleDeps
  , Compile
  , runCompile
  , compileModule
  ) where

import Data.List (nub, (\\))

import System.FilePath ((</>), replaceExtension, takeDirectory)

import Control.Monad.Trans.Reader (ReaderT, runReaderT)
import Control.Monad.Supply (SupplyT, evalSupplyT, runSupplyT)
import Data.Either (partitionEithers)
import Language.PureScript.Errors (ErrorMessage( SimpleErrorWrapper ), singleError, SimpleErrorMessage (CannotWriteFile, UnnecessaryFFIModule, ErrorParsingFFIModule), errorMessage)
import Data.Maybe (mapMaybe, isJust)
import System.Directory
       (doesFileExist, getModificationTime, createDirectoryIfMissing)
import System.IO.Error (tryIOError)
import qualified Data.ByteString.Char8 as B
import Language.PureScript.Sugar (desugar)
import Control.Monad (when)
import qualified Language.PureScript.Constants as C
import Language.PureScript.AST.Declarations (Declaration ( ImportDeclaration ))
import Control.Monad.Error.Class (MonadError, throwError)
import Language.PureScript.TypeChecker.Monad (runCheck')
import Language.PureScript.TypeChecker (typeCheckModule)
import Language.PureScript.Pretty.JS (prettyPrintJS)
import Language.PureScript.Parser.JS (findModuleName)
import qualified Language.PureScript.CodeGen.JS as J
import Language.PureScript.AST.Declarations (ImportDeclarationType( Implicit ), Declaration ( PositionedDeclaration))
import Language.PureScript.Linter.Exhaustive (checkExhaustiveModule)
import qualified Data.Set as S
import Language.PureScript.Errors (MultipleErrors)
import Language.PureScript.Options (Options)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Writer (WriterT, runWriterT)
import Language.PureScript.Sugar.BindingGroups (createBindingGroups, collapseBindingGroups)
import qualified Language.PureScript.CoreFn as CF
import Language.PureScript.Renamer (renameInModules)
import Language.PureScript.CodeGen.Externs (moduleToPs)
import Control.Monad.Supply.Class (fresh)
import Control.Monad.Trans.Except (ExceptT, runExceptT)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Writer.Class (MonadWriter, tell)
import Language.PureScript.Environment (Environment, initEnvironment)
import Language.PureScript.Names (runModuleName, ModuleName( ModuleName ), ProperName (ProperName))
import Language.PureScript.AST.Declarations (Module ( Module ))
import Control.Monad.Reader.Class (MonadReader)
import Language.PureScript.ModuleDependencies (usedModules, sortModules)
import qualified Language.PureScript as P
import Language.PureScript.Linter (lint)

import Debug.Trace

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

compileIO :: (IOError -> ErrorMessage) -> IO a -> Compile a
compileIO f io = do
  e <- liftIO $ tryIOError io
  either (throwError . singleError . f) return e

--compileModule :: (MonadError MultipleErrors m, MonadWriter MultipleErrors m) => Module -> m b
compileModule input outputDir usePrefix m@(Module _ _ ds _) = do
  liftIO $ print $ "All imported modules: " ++ show (importedModules ds)
  allModules <- loadAllModules (importedModules ds)
  (sorted, _) <- sortModules $ map importPrim allModules
  liftIO $ print "--- 1 ---"
  lint m
  liftIO $ print "--- 2 ---"
  (allDesugared, nextVar) <- runSupplyT 0 $ desugar (sorted ++ [importPrim m])
  liftIO $ print "--- 3 ---"
  env' <- typeCheckAllModules initEnvironment (init allDesugared)
  evalSupplyT nextVar $ go env' (last allDesugared)

  where
  go env m@(Module coms moduleName' _ exps) = do
    lift $ progress $ "Compiling " ++ runModuleName moduleName'
    (checked@(Module _ _ elaborated _), env') <- lift . runCheck' env $ typeCheckModule Nothing m
    checkExhaustiveModule env' checked
    regrouped <- createBindingGroups moduleName' . collapseBindingGroups $ elaborated
    let mod' = Module coms moduleName' regrouped exps
        corefn = CF.moduleToCoreFn env' mod'
        [renamed] = renameInModules [corefn]
        exts = moduleToPs mod' env'
    nextVar <- fresh
    lift $ codegen renamed env' nextVar exts

--  loadAllModules :: Monad m => [ModuleName] -> m [Module]
  loadAllModules names = loadAllModules' S.empty [] names

  loadAllModules' loadedNames loadedModules [] = return loadedModules
  loadAllModules' loadedNames loadedModules (x:xs) = do
    let modNameStr = runModuleName x
    if modNameStr == "Prim" || S.member x loadedNames then loadAllModules' loadedNames loadedModules xs
      else do
          let filePath = modNameStr
              externsFile = outputDir </> filePath </> "externs.purs"
          moduleFile <- liftIO $ readFile externsFile
          [(_, m@(Module _ _ ds _))] <- P.parseModulesFromFiles id [(externsFile, moduleFile)]
          loadAllModules' (S.insert x loadedNames) (m:loadedModules) (xs ++ importedModules ds)

  typeCheckAllModules env [] = return env
  typeCheckAllModules env (m:ms) = do
    (_, env') <- runCheck' env $ typeCheckModule Nothing m
    typeCheckAllModules env' ms

  codegen :: CF.Module CF.Ann -> Environment -> SupplyVar -> Externs -> Compile ()
  codegen m _ nextVar exts = do
    let mn = CF.moduleName m
    let jsfile = (input `replaceExtension` "js")
    (foreignInclude, jsContents) <- case requiresForeign m of
      True -> do
        jsContents <- liftIO $ readFile jsfile
        case findModuleName (lines jsContents) of
          Nothing -> throwError (errorMessage $ ErrorParsingFFIModule jsfile)
          Just n -> do
            when (n /= mn) $ throwError (error $ "TODO Incorrect FFI Module Name: " ++ show n)
            return ( Just $ J.JSApp (J.JSVar "require") [J.JSStringLiteral "./foreign"]
                   , jsContents)
      False -> do
        ffiExists <- liftIO $ doesFileExist jsfile
        when ffiExists $ tell $ errorMessage $ UnnecessaryFFIModule mn jsfile
        return (Nothing, "")
    pjs <- evalSupplyT nextVar $ prettyPrintJS <$> J.moduleToJs m foreignInclude
    let filePath = runModuleName mn
        jsFile = outputDir </> filePath </> "index.js"
        externsFile = outputDir </> filePath </> "externs.purs"
        foreignFile = outputDir </> filePath </> "foreign.js"
        prefix = ["Generated by psc version " ++ "XXX" | usePrefix]
        js = unlines $ map ("// " ++) prefix ++ [pjs]
    writeTextFile jsFile js
    when (isJust foreignInclude) $ (writeTextFile foreignFile jsContents)
    -- Only write to the "externs.purs" if it is changed
    mbOldExts <- liftIO $ maybeReadTextFile externsFile
    case mbOldExts of
      Just oldExts -> when (oldExts /= exts) $
        writeTextFile externsFile exts
      Nothing ->
        writeTextFile externsFile exts

  requiresForeign :: CF.Module a -> Bool
  requiresForeign = not . null . CF.moduleForeign


maybeReadTextFile :: FilePath -> IO (Maybe String)
maybeReadTextFile path = do
  result <- tryIOError $ B.readFile path
  case result of
    Left _ -> return Nothing
    Right file -> return (Just (B.unpack file))


writeTextFile :: FilePath -> String -> Compile ()
writeTextFile path text = compileIO (const (SimpleErrorWrapper $ CannotWriteFile path)) $ do
  mkdirp path
  putStrLn $ "Writing " ++ path
  writeFile path text
  where
  mkdirp :: FilePath -> IO ()
  mkdirp = createDirectoryIfMissing True . takeDirectory

progress :: String -> Compile ()
progress = liftIO . putStrLn


-- |
-- A value to be used in the Supply monad.
--
type SupplyVar = Integer

-- |
-- Generated code for an externs file.
--
type Externs = String


------------------------------------------------------------------------


getModuleDeps :: FilePath -> FilePath -> Module -> [FilePath]
getModuleDeps input outputDir m =
  let importFiles = getImports m
      foreignFiles = getForeign m
  in importFiles ++ foreignFiles

  where
  getImports :: Module -> [FilePath]
  getImports (Module _ _ ds _) =
    let moduleNames = map runModuleName (importedModules ds)
        toFilePath m = outputDir </> m </> "externs.purs"
    in nub (map toFilePath moduleNames)

  getForeign :: Module -> [FilePath]
  getForeign (Module _ _ ds _) =
    case moduleHasForeign ds of
      True -> [input `replaceExtension` "js"]
      False -> []


importedModules :: [Declaration] -> [ModuleName]
importedModules ds =
    let modsX = (concatMap usedModulesX ds)
        (lefts, rights) = partitionEithers modsX
    in (nub rights) \\ (nub lefts)

usedModulesX :: Declaration -> [Either ModuleName ModuleName]
usedModulesX = let (f, _, _, _, _) = P.everythingOnValues (++) forDecls forValues (const []) (const []) (const []) in nub . f
  where
  forDecls :: Declaration -> [Either ModuleName ModuleName]
  forDecls (ImportDeclaration mn _ Nothing) = [Right mn]
  forDecls (ImportDeclaration mn _ (Just q)) = [Right mn, Left q]
  forDecls _ = []

  forValues :: P.Expr -> [Either ModuleName ModuleName]
  forValues (P.Var (P.Qualified (Just mn) _)) = [Right mn]
  forValues (P.Constructor (P.Qualified (Just mn) _)) = [Right mn]
  forValues (P.TypedValue _ _ ty) = forTypes ty
  forValues _ = []

  forTypes :: P.Type -> [Either ModuleName ModuleName]
  forTypes (P.TypeConstructor (P.Qualified (Just mn) _)) = [Right mn]
  forTypes (P.ConstrainedType cs _) = mapMaybe (\(P.Qualified mn _, _) -> maybe Nothing (\x -> Just (Right x)) mn) cs
  forTypes _ = []

-- |
-- Check if the module contains any foreign declarations
--
moduleHasForeign :: [Declaration] -> Bool
moduleHasForeign ds = not (null (concatMap foreigns ds)) -- TODO This implementation is very ugly (and probably inefficient)
  where
  foreigns :: Declaration -> [()]
  foreigns = let (f, _, _, _, _) = P.everythingOnValues (++) forDecls (const []) (const []) (const []) (const []) in nub . f

  forDecls :: Declaration -> [()]
  forDecls (P.ExternDeclaration _ _) = [()]
  forDecls (P.ExternInstanceDeclaration _ _ _ _) = [()]
  forDecls _ = []


------------------------------------------------------------------------


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
