module MicroCabal.Backend.GHC(ghcBackend) where
import Control.Monad
import Data.List
import Data.Version
import System.Directory
import MicroCabal.Cabal
import MicroCabal.Env
import MicroCabal.Macros
import MicroCabal.Parse(readVersion)
import MicroCabal.Unix

ghcBackend :: Env -> IO Backend
ghcBackend env = do
  -- Actual GHC version.
  numVersion <- takeWhile (/= '\n') <$> cmdOut env "ghc --numeric-version"
  -- GHC version used in the stackage snapshot.
  snapVersion <- readFile (cabalDir env </> "ghc-version")
  let ghcVersion = "ghc-" ++ numVersion
      version = readVersion numVersion
  -- Check that the ghc version is the one that the Stackage snapshot wants.
  when (snapVersion /= ghcVersion) $
    error $ "The Stackage snapshot files are for " ++ snapVersion ++ ", but the current compiler is " ++ ghcVersion

  return Backend {
    compilerName = "ghc",
    compilerVersion = version,
    compiler = ghcVersion,
    doesPkgExist = ghcExists,
    buildPkgExe = ghcBuildExe,
    buildPkgLib = ghcBuildLib,
    installPkgExe = ghcInstallExe,
    installPkgLib = ghcInstallLib
    }

getGhcName :: Env -> IO FilePath
getGhcName env = return $ compiler $ backend env

getGhcDir :: Env -> IO FilePath
getGhcDir env = (cabalDir env </>) <$> getGhcName env

getBuildDir :: Env -> IO FilePath
getBuildDir env = do
  ghc <- getGhcName env
  return $ distDir env </> "build" </> ghc

initDB :: Env -> IO ()
initDB env = do
  dir <- getGhcDir env
  b <- doesDirectoryExist dir
  when (not b) $ do
    message env 0 $ "Creating GHC package db " ++ dir
    cmd env $ "ghc-pkg init " ++ dir

ghcExists :: Env -> PackageName -> IO Bool
ghcExists env pkgname = do
  -- First try with globally installed GHC packages.
  ok <- tryCmd env $ "ghc-pkg describe >/dev/null 2>/dev/null " ++ pkgname
  if ok then
    return True
   else do
    -- Try with packages installed wikh mcabal
    dir <- getGhcDir env
    tryCmd env $ "ghc-pkg --package-db=" ++ dir ++ " describe >/dev/null 2>/dev/null " ++ pkgname

setupStdArgs :: Env -> [Field] -> IO [String]
setupStdArgs env flds = do
  db <- getGhcDir env
  let srcDirs = getFieldStrings flds ["."]   "hs-source-dirs"
      defExts = getFieldStrings flds []      "default-extensions"
      exts    = getFieldStrings flds defExts "extensions"
      opts    = getFieldStrings flds []      "ghc-options"
      cppOpts = getFieldStrings flds []      "cpp-options"
      incDirs = getFieldStrings flds []      "include-dirs"
      mlang   = getFieldStringM flds         "default-language"
      deps    = getBuildDependsPkg flds
      lang    = maybe [] (\ s -> ["-X" ++ s]) mlang
  buildDir <- getBuildDir env
  depvers <- mapM (getPackageVersion env) deps
  let macros = genPkgVersionMacros (zip deps depvers)
  return $
    [ "-package-env=-",
      "-package-db=" ++ db,
      "-outputdir=" ++ buildDir,
      "-w"] ++
    map ("-i" ++) srcDirs ++
    ["-i" ++ pathModuleDir env] ++
    map ("-I" ++) incDirs ++
    map ("-X" ++) exts ++
    lang ++
    map ("-package " ++) deps ++
    opts ++
    macros ++
    cppOpts

binGhc :: FilePath
binGhc  = "bin" </> "ghc"

ghcBuildExe :: Env -> Section -> Section -> IO ()
ghcBuildExe env _ (Section _ name flds) = do
  initDB env
  let mainIs  = getFieldString flds "main-is"
      srcDirs = getFieldStrings flds ["."] "hs-source-dirs"
      bin     = distDir env </> binGhc </> name
  mkdir env $ distDir env </> binGhc
  mainIs' <- findMainIs env srcDirs mainIs
  stdArgs <- setupStdArgs env flds
  let args    = unwords $ ["-O"] ++ stdArgs ++ ["-o", bin, "--make", mainIs'] ++
                [ ">/dev/null" | verbose env <= 0 ]
  message env 0 $ "Building executable " ++ bin ++ " with ghc"
  cmd env $ "ghc " ++ args

findMainIs :: Env -> [FilePath] -> FilePath -> IO FilePath
findMainIs _ [] fn = error $ "cannot find " ++ show fn
findMainIs env (d:ds) fn = do
  let fn' = d </> fn
  b <- doesFileExist fn'
  if b then
    return fn'
   else
    findMainIs env ds fn

-- It can happen that there are no exposed modules.
-- E.g., for a package only needed for certain versions.
getExposedModules :: [Field] -> [String]
getExposedModules flds = getFieldStrings flds [] "exposed-modules"

getOtherModules :: [Field] -> [String]
getOtherModules flds = getFieldStrings flds [] "other-modules"

ghcBuildLib :: Env -> Section -> Section -> IO ()
ghcBuildLib env (Section _ _ glob) (Section _ name flds) = do
  initDB env
  stdArgs <- setupStdArgs env flds
  let emdls = getExposedModules flds
      omdls = getOtherModules flds
      mdls = emdls ++ omdls
      ver  = getVersion glob "version"
      args = unwords $ ["-O"] ++ stdArgs ++
                       ["--make", "-no-link", "-this-unit-id", key ] ++
                       ["-fbuilding-cabal-package", "-static" ] ++
                       mdls ++
                       [ ">/dev/null" | verbose env <= 0 ]
      key = name ++ "-" ++ showVersion ver ++ "-mcabal"
  if null mdls then
    message env 0 $ "Building library " ++ name ++ " with ghc skipped, no modules"
   else do
    message env 0 $ "Building library " ++ name ++ " with ghc"
    cmd env $ "ghc " ++ args

ghcInstallExe :: Env -> Section -> Section -> IO ()
ghcInstallExe env (Section _ _ _glob) (Section _ name _) = do
  let bin = distDir env ++ binGhc ++ name
      binDir = cabalDir env </> "bin"
  mkdir env binDir
  cpr env bin (binDir </> name)

getPackageField :: String -> Env -> PackageName -> IO PackageName
getPackageField fld env n = do
  mr <- tryCmdOut env $ "ghc-pkg field " ++ n ++ " " ++ fld ++ " 2>/dev/null"  -- returns "fld: val"
  last . words <$>
    case mr of
      Just r -> return r
      Nothing -> do
        dir <- getGhcDir env
        cmdOut env ("ghc-pkg field  --package-db=" ++ dir ++ " " ++ n ++ " " ++ fld)  -- returns "fld: val"

getPackageId :: Env -> PackageName -> IO PackageName
getPackageId = getPackageField "id"

getPackageVersion :: Env -> PackageName -> IO Version
getPackageVersion env n = readVersion <$> getPackageField "version" env n

ghcInstallLib :: Env -> Section -> Section -> IO ()
ghcInstallLib env (Section _ _ glob) (Section _ name flds) = do
  initDB env
  buildDir <- getBuildDir env
  ghc <- getGhcDir env
  let namever = name ++ "-" ++ showVersion vers
      destDir = ghc </> namever
      vers = getVersion glob "version"
      archOut = destDir </> "libHS" ++ namever ++ "-mcabal.a"
  mkdir env destDir
  rmrf env archOut

  let files = map mdlToHi (omdls ++ mdls)
      mdls = getExposedModules flds
      omdls = getOtherModules flds
      mdlToHi = (++ ".hi") . map (\ c -> if c == '.' then '/' else c)

  when (not (null files)) $ do
    cmd env $ "ar -c -r -s " ++ archOut ++ " `find " ++ buildDir ++ " -name '*.o'`"
    copyFiles env buildDir files destDir

  db <- getGhcDir env
  let extraLibs = getFieldStrings flds [] "extra-libraries"
      deps = getBuildDependsPkg flds
  depends <- nub <$> mapM (getPackageId env) deps
  let desc = unlines $
        [ "name: " ++ name
        , "version: " ++ showVersion vers
        , "visibility: public"
        , "id: " ++ key
        , "key: " ++ key
        , "exposed: True"
        , "exposed-modules: " ++ unwords mdls
        , "import-dirs: " ++ destDir
        , "library-dirs: " ++ destDir
        , "library-dirs-static: " ++ destDir
        , "extra-libraries: " ++ unwords extraLibs
        , "depends: " ++ unwords depends
        ] ++
        [ "hs-libraries: HS" ++ key | not (null files) ]
      key = namever ++ "-mcabal"
      pkgFn = db </> key ++ ".conf"
      quiet = if verbose env > 0 then "" else " >/dev/null"
  writeFile pkgFn desc
  cmd env $ "ghc-pkg update --package-db=" ++ db ++ " " ++ pkgFn ++ quiet
