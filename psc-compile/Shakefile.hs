-------------------------------------------------------------------------------
--
--
-- This is a sample Shakefile.hs that can be used to build a PureScript project
-- using psc-compile.
--
-- By default it requires a file called "purescript_paths.txt" containing a
-- list of all the source paths that should be searched for PureScript modules.
--
-- Example:
--
--     $ cat purescript_paths.txt
--     purescript-prelude/src
--     purescript-eff/src
--     src
--
-- By default it required a main PureScript module named "Main".  By default it
-- will compile the PureScript modules into the directory `_build/psc-output`
-- and will call psc-bundle to create a final app js bundle to the file
-- `_build/app.js`
--
-- The script reads the following environment variables:
--
--     PSC_COMPILE (default is "psc-compile")
--     PSC_BUNDLE (default is "psc-bundle")
--
-- To run this Shakefile.hs make sure you have GHC installed and the shake
-- package and run:
--
--      $ runghc Shakefile.hs
--
-- If you you need to set command line flags for psc-compile then you can
-- modify the invocation directly, although this functionality should probably
-- be exposed in the PureScriptBuild configuration data.
--
-------------------------------------------------------------------------------

import Development.Shake
import Development.Shake.Command
import Development.Shake.FilePath
import Development.Shake.Util

import Control.Monad (forM_, when)
import qualified System.Directory as D

-------------------------------------------------------------------------------
-- Build Configuration
-------------------------------------------------------------------------------

buildDir = "_build"

-------------------------------------------------------------------------------
-- Build Rules
-------------------------------------------------------------------------------

main :: IO ()
main = do
    -- Read the list of search paths for PureScript Modules from the following
    -- file (one per line):
    pscSourceDirs <- (fmap lines . readFile) "purescript_paths.txt"

    shakeArgs shakeOptions{shakeFiles=buildDir} $ do
        cleanRule

        -----------------------------------------------------------------------
        -- PureScript Build:
        -----------------------------------------------------------------------

        pureScriptBuild $ PureScriptBuild
            { pscWantOutputJs = True
            , pscOutputJs = buildDir </> "app.js"
            , pscOutputDir = buildDir </> "psc-output"
            , pscSourceDirs = pscSourceDirs
            , pscMainModule = "Main"
            }

        -----------------------------------------------------------------------
        -- End of Build Rules
        -----------------------------------------------------------------------

    where
    cleanRule = do
        phony "clean" $ do
            putNormal $ "Cleaning files in " ++ buildDir
            removeFilesAfter buildDir ["//*"]



-------------------------------------------------------------------------------
-- PureScript Build functionality
-------------------------------------------------------------------------------

type PSModuleName = String

data PureScriptBuild = PureScriptBuild
    { pscWantOutputJs :: Bool
    , pscOutputJs :: FilePath
    , pscOutputDir :: FilePath
    , pscSourceDirs :: [FilePath]
    , pscMainModule :: PSModuleName
    }

pureScriptBuild (PureScriptBuild wantOutputJs outputJs outputDir sourceDirs mainModule) = do
    when wantOutputJs $
        want [outputJs]

    outputJs %> \out -> do
        need [outputDir </> mainModule </> "index.js"]
        -- TODO This is a bit of a hack: We are telling shake to depend on
        -- *all* of the files we can find in all of the source dirs. The
        -- correct solution is to be able to track which of the files were used
        -- by psc-bundle. (but psc-bundle currently does not offer a way to
        -- glean this information).
        forM_ sourceDirs $ \sourceDir -> do
            files <- getDirectoryFiles "" [sourceDir <//> "*.purs", sourceDir <//> "*.js"]
            need files
        pscBundle <- get_PSC_BUNDLE
        () <- cmd [pscBundle] [outputDir </> "**" </> "*.js"] "-m" [mainModule] "--main" [mainModule] "-o" [out]
        return ()

    outputDir </> "*" </> "externs.purs" %> \outexterns -> do
        let outdeps = takeDirectory outexterns </> "index.deps"
        need [outdeps]

    [outputDir </> "*" </> "index.js", outputDir </> "*" </> "index.deps"] &%> \[outjs, outdeps] -> do
        let moduleName = (dropDirPrefix outputDir) (takeDirectory outjs)
        sourceFile <- liftIO $ findPscSourceFile moduleName
        pscCompile <- get_PSC_COMPILE
        (Stdout out) <- cmd [pscCompile] "--deps" [sourceFile] "--output" [outputDir]
        writeFile' outdeps out
        need [sourceFile]
        deps <- liftIO $ (fmap lines . readFile) outdeps
        need deps
        () <- cmd [pscCompile] [sourceFile] "--output" [outputDir]
        return ()

    where
    findPscSourceFile :: PSModuleName -> IO FilePath
    findPscSourceFile moduleName = do
        let path = moduleToPath moduleName <.> "purs"
        search <- mapM (checkFile path) sourceDirs
        let results = concat search

        when (null results) $
            fail $ "PureScript module \"" ++ moduleName ++ "\" Not found in any of the search paths"

        when ((not . null) (tail results)) $
            fail $ "Multiple conflicting source files found for PureScript module \"" ++ moduleName ++ "\": " ++ show results

        return (head results)
        where
        moduleToPath :: PSModuleName -> FilePath
        moduleToPath moduleName = foldr1 (</>) (splitBy '.' moduleName)

        checkFile :: FilePath -> FilePath -> IO [FilePath]
        checkFile subPath sourceDir = do
            exists <- D.doesFileExist (sourceDir </> subPath)
            if exists
                then return [sourceDir </> subPath]
                else return []

    get_PSC_COMPILE :: Action String
    get_PSC_COMPILE = getEnvWithDefault "psc-compile" "PSC_COMPILE"

    get_PSC_BUNDLE :: Action String
    get_PSC_BUNDLE = getEnvWithDefault "psc-bundle" "PSC_BUNDLE"


-------------------------------------------------------------------------------
-- Helpers Functions
-------------------------------------------------------------------------------

splitBy :: Eq a => a -> [a] -> [[a]]
splitBy delimiter = foldr f [[]]
    where f c l@(x:xs) | c == delimiter = []:l
                       | otherwise = (c:x):xs

dropDirPrefix :: FilePath -> FilePath -> FilePath
dropDirPrefix "" path = path
dropDirPrefix dirPrefix path | isPathSeparator (last dirPrefix) = drop (length dirPrefix) path
                             | otherwise = drop ((length dirPrefix) + 1) path

