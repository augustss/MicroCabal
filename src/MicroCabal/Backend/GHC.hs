module MicroCabal.Backend.GHC(ghcBackend) where
import Control.Monad
import Data.List
import Data.Version
import System.Directory
import MicroCabal.Cabal
import MicroCabal.Env
import MicroCabal.Parse(readVersion)
import MicroCabal.Unix

ghcBackend :: Backend
ghcBackend = Backend {
  backendNameVers = ghcNameVers,
  doesPkgExist = ghcExists,
  buildPkgExe = ghcBuildExe,
  buildPkgLib = ghcBuildLib,
  installPkgExe = ghcInstallExe,
  installPkgLib = ghcInstallLib
  }

ghcNameVers :: Env -> IO (String, Version)
ghcNameVers env = do
  svers <- takeWhile (/= '\n') <$> cmdOut env "ghc --numeric-version"
  -- Check that the ghc version is the one that the Stackage snapshot wants.
  -- XXX This should be somewhere else.
  snapVersion <- readFile (cabalDir env </> "ghc-version")
  let ghcVersion = "ghc-" ++ svers
  when (snapVersion /= ghcVersion) $
    error $ "The Stackage snapshot files are for " ++ snapVersion ++ ", but the current compiler is " ++ ghcVersion
  return ("ghc", readVersion svers)

getGhcName :: Env -> IO FilePath
getGhcName env = do
  (n, v) <- ghcNameVers env
  return $ n ++ "-" ++ showVersion v

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
    when (verbose env > 0) $
      putStrLn $ "Creating GHC package db " ++ dir
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
      deps    = getBuildDependsPkg flds
  buildDir <- getBuildDir env
  return $ [ "-package-env=-", "-package-db=" ++ db, "-outputdir=" ++ buildDir, "-w"] ++
           map ("-i" ++) srcDirs ++
           map ("-X" ++) exts ++
           map ("-package " ++) deps ++
           opts ++ cppOpts

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
  let args    = unwords $ ["-O"] ++ stdArgs ++ ["-o", bin, "--make", mainIs']
  when (verbose env >= 0) $
    putStrLn $ "Build executable " ++ bin ++ " with ghc"
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

getExposedModules :: [Field] -> [String]
getExposedModules flds = getFieldStrings flds (error "no exposed-modules") "exposed-modules"

getOtherModules :: [Field] -> [String]
getOtherModules flds = getFieldStrings flds [] "other-modules"

ghcBuildLib :: Env -> Section -> Section -> IO ()
ghcBuildLib env (Section _ _ glob) (Section _ name flds) = do
  initDB env
  stdArgs <- setupStdArgs env flds
  let mdls = getExposedModules flds
      omdls = getOtherModules flds
      ver  = getVersion glob "version"
      args = unwords $ ["-O"] ++ stdArgs ++
                       ["--make", "-no-link", "-this-unit-id", key ] ++
                       ["-fbuilding-cabal-package", "-static" ] ++
                       (omdls ++ mdls)
      key = name ++ "-" ++ showVersion ver ++ "-mcabal"
  when (verbose env >= 0) $
    putStrLn $ "Build library " ++ name ++ " with ghc"
  cmd env $ "ghc " ++ args

ghcInstallExe :: Env -> Section -> Section -> IO ()
ghcInstallExe env (Section _ _ _glob) (Section _ name _) = do
  let bin = distDir env ++ binGhc ++ name
      binDir = cabalDir env </> "bin"
  mkdir env binDir
  cp env bin (binDir </> name)

getPackageId :: Env -> PackageName -> IO PackageName
getPackageId env n = do
  mr <- tryCmdOut env $ "ghc-pkg field " ++ n ++ " id 2>dev/null"  -- returns "id: pkg-id"
  case mr of
    Just r -> return $ last $ words r
    Nothing -> do
      dir <- getGhcDir env
      last . words <$> cmdOut env ("ghc-pkg field  --package-db=" ++ dir ++ " " ++ n ++ " id")  -- returns "id: pkg-id"

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
  cmd env $ "ar -c -r -s " ++ archOut ++ " `find " ++ buildDir ++ " -name '*.o'`"

  let files = map mdlToHi (omdls ++ mdls)
      mdls = getExposedModules flds
      omdls = getOtherModules flds
      mdlToHi = (++ ".hi") . map (\ c -> if c == '.' then '/' else c)
  copyFiles env buildDir files destDir

  db <- getGhcDir env
  let extraLibs = getFieldStrings flds [] "extra-libraries"
      deps = getBuildDependsPkg flds
  depends <- nub <$> mapM (getPackageId env) deps
  let desc = unlines
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
        , "hs-libraries: HS" ++ key
        , "extra-libraries: " ++ unwords extraLibs
        , "depends: " ++ unwords depends
        ]
      key = namever ++ "-mcabal"
      pkgFn = db </> key ++ ".conf"
  writeFile pkgFn desc
  cmd env $ "ghc-pkg update --package-db=" ++ db ++ " " ++ pkgFn
