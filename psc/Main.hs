-----------------------------------------------------------------------------
--
-- Module      :  Main
-- Copyright   :  (c) Phil Freeman 2013
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

import Control.Applicative
import Control.Monad
import Control.Monad.Error.Class (MonadError(..))
import Control.Monad.Writer

import Data.Version (showVersion)
import qualified Data.Map as M

import Options.Applicative as Opts

import System.Exit (exitSuccess, exitFailure)
import System.IO (hPutStrLn, stderr)
import System.FilePath.Glob (glob)

import qualified Language.PureScript as P
import Language.PureScript.OptionsParsing (options)
--import qualified Paths_purescript as Paths

import Language.PureScript.Make

data PSCMakeOptions = PSCMakeOptions
  { pscmInput        :: [FilePath]
  , pscmForeignInput :: [FilePath]
  , pscmOutputDir    :: FilePath
  , pscmOpts         :: P.Options
  , pscmUsePrefix    :: Bool
  }

data InputOptions = InputOptions
  { ioInputFiles  :: [FilePath]
  }

compile :: PSCMakeOptions -> IO ()
compile (PSCMakeOptions inputGlob inputForeignGlob outputDir opts usePrefix) = do
  input <- concat <$> mapM glob inputGlob
  when (null input) $ do
    hPutStrLn stderr "psc: No input files."
    exitFailure
  moduleFiles <- readInput (InputOptions input)
  print moduleFiles
  inputForeign <- concat <$> mapM glob inputForeignGlob
  foreignFiles <- forM inputForeign (\inFile -> (inFile,) <$> readFile inFile)
  case runWriterT (parseInputs moduleFiles foreignFiles) of
    Left errs -> do
      hPutStrLn stderr (P.prettyPrintMultipleErrors (P.optionsVerboseErrors opts) errs)
      exitFailure
    Right ((ms, foreigns), warnings) -> do
      when (P.nonEmpty warnings) $
        hPutStrLn stderr (P.prettyPrintMultipleWarnings (P.optionsVerboseErrors opts) warnings)
      let filePathMap = M.fromList $ map (\(fp, P.Module _ mn _ _) -> (mn, fp)) ms
          makeActions = buildMakeActions outputDir filePathMap foreigns usePrefix
      e <- runMake opts $ P.make makeActions ms
      case e of
        Left errs -> do
          hPutStrLn stderr (P.prettyPrintMultipleErrors (P.optionsVerboseErrors opts) errs)
          exitFailure
        Right (_, warnings') -> do
          when (P.nonEmpty warnings') $
            putStrLn (P.prettyPrintMultipleWarnings (P.optionsVerboseErrors opts) warnings')
          exitSuccess

readInput :: InputOptions -> IO [(Either P.RebuildPolicy FilePath, String)]
readInput InputOptions{..} = forM ioInputFiles $ \inFile -> (Right inFile, ) <$> readFile inFile

parseInputs :: (Functor m, Applicative m, MonadError P.MultipleErrors m, MonadWriter P.MultipleErrors m)
            => [(Either P.RebuildPolicy FilePath, String)]
            -> [(FilePath, P.ForeignJS)]
            -> m ([(Either P.RebuildPolicy FilePath, P.Module)], M.Map P.ModuleName (FilePath, P.ForeignJS))
parseInputs modules foreigns =
  (,) <$> P.parseModulesFromFiles (either (const "") id) modules
      <*> P.parseForeignModulesFromFiles foreigns

inputFile :: Parser FilePath
inputFile = strArgument $
     metavar "FILE"
  <> help "The input .purs file(s)"

inputForeignFile :: Parser FilePath
inputForeignFile = strOption $
     short 'f'
  <> long "ffi"
  <> help "The input .js file(s) providing foreign import implementations"

outputDirectory :: Parser FilePath
outputDirectory = strOption $
     short 'o'
  <> long "output"
  <> Opts.value "output"
  <> showDefault
  <> help "The output directory"

noPrefix :: Parser Bool
noPrefix = switch $
     short 'p'
  <> long "no-prefix"
  <> help "Do not include comment header"


                    
pscMakeOptions :: Parser PSCMakeOptions
pscMakeOptions = PSCMakeOptions <$> many inputFile
                                <*> many inputForeignFile
                                <*> outputDirectory
                                <*> options
                                <*> (not <$> noPrefix)

main :: IO ()
main = execParser opts >>= compile
  where
  opts        = info (version <*> helper <*> pscMakeOptions) infoModList
  infoModList = fullDesc <> headerInfo <> footerInfo
  headerInfo  = header   "psc - Compiles PureScript to Javascript"
  footerInfo  = footer $ "psc " ++ "XXX"

  version :: Parser (a -> a)
  version = abortOption (InfoMsg "XXX") $ long "version" <> help "Show the version number" <> hidden
