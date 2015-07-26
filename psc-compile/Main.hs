-----------------------------------------------------------------------------
--
-- Module      :  Main
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
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module Main where

import Control.Monad.Error.Class (MonadError(..))
import Control.Monad.Writer

import Options.Applicative as Opts

import System.Exit (exitSuccess, exitFailure)
import System.IO (hPutStrLn, stderr)
import System.FilePath ((</>))

import qualified Language.PureScript as P
import Language.PureScript.OptionsParsing (options)
import Language.PureScript.Compile (getModuleDeps, runCompile, compileModule)

data PSCCompileOptions = PSCCompileOptions
  { psccInput     :: FilePath
  , psccOutputDir :: FilePath
  , psccOpts      :: P.Options
  , psccGetDeps   :: Bool
  , psccUsePrefix :: Bool
  }

compile :: PSCCompileOptions -> IO ()
compile (PSCCompileOptions input outputDir opts onlyDeps usePrefix) = do
  when (not onlyDeps) $ putStrLn "Loading from disk..."
  moduleFile <- readInput input
  when (not onlyDeps) $ putStrLn "Parsing..."
  case runWriterT (parseInput (input, moduleFile)) of
    Left errs -> do
      hPutStrLn stderr (P.prettyPrintMultipleErrors (P.optionsVerboseErrors opts) errs)
      exitFailure
    Right (m, warnings) -> do
      when (not onlyDeps) $ putStrLn "Done parsing..."
      if onlyDeps then mapM_ putStrLn (getModuleDeps input outputDir m) else do
        when (P.nonEmpty warnings) $
          hPutStrLn stderr (P.prettyPrintMultipleWarnings (P.optionsVerboseErrors opts) warnings)
        putStrLn "Getting ready..."
        e <- runCompile opts (compileModule outputDir usePrefix m)
        case e of
          Left errs -> do
            hPutStrLn stderr (P.prettyPrintMultipleErrors (P.optionsVerboseErrors opts) errs)
            exitFailure
          Right (_, warnings') -> do
            when (P.nonEmpty warnings') $
              putStrLn (P.prettyPrintMultipleWarnings (P.optionsVerboseErrors opts) warnings')
            exitSuccess

readInput :: FilePath -> IO String
readInput = readFile

parseInput :: (Functor m, Applicative m, MonadError P.MultipleErrors m, MonadWriter P.MultipleErrors m)
           => (FilePath, String)
           -> m P.Module
parseInput nameAndContents = do
  [(_, m)] <- P.parseModulesFromFiles id [nameAndContents]
  return m


inputFile :: Parser FilePath
inputFile = strArgument $
     metavar "FILE"
  <> help "The input .purs file"

outputDirectory :: Parser FilePath
outputDirectory = strOption $
     short 'o'
  <> long "output"
  <> Opts.value "output"
  <> showDefault
  <> help "The output directory. Note that this is also the directory that will be looked in for `externs.purs' files belonging to previously compiled modules."

getDeps :: Parser Bool
getDeps = switch $
    short 'd'
  <> long "deps"
  <> help "Do not compile. Instead, print list of files that the module depends on (to stdout)"

noPrefix :: Parser Bool
noPrefix = switch $
     short 'p'
  <> long "no-prefix"
  <> help "Do not include comment header"

pscCompileOptions :: Parser PSCCompileOptions
pscCompileOptions = PSCCompileOptions <$> inputFile
                                      <*> outputDirectory
                                      <*> options
                                      <*> getDeps
                                      <*> (not <$> noPrefix)

main :: IO ()
main = execParser opts >>= compile
  where
  opts        = info (version <*> helper <*> pscCompileOptions) infoModList
  infoModList = fullDesc <> headerInfo <> footerInfo
  headerInfo  = header   "psc-compile - Compiles a single PureScript module to Javascript"
  footerInfo  = footer $ "psc-compile " ++ "XXX"

  version :: Parser (a -> a)
  version = abortOption (InfoMsg "XXX") $ long "version" <> help "Show the version number" <> hidden
