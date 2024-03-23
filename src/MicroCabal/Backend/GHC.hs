module MicroCabal.Backend.GHC(ghcBackend) where
import Control.Monad
import System.Directory
import MicroCabal.Cabal
import MicroCabal.Env
import MicroCabal.Unix

ghcBackend :: Backend
ghcBackend = Backend {
  backendName = "ghc",
  doesPkgExist = ghcExists,
  buildPkgExe = ghcBuildExe,
  buildPkgLib = ghcBuildLib,
  installPkgLib = ghcInstallLib }

getGhcName :: Env -> IO FilePath
getGhcName env = ("ghc-" ++) . takeWhile (/= '\n') <$> cmdOut env "ghc --numeric-version"

getGhcDir :: Env -> IO FilePath
getGhcDir env = ((cabalDir env ++ "/") ++) <$> getGhcName env

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
  ghc <- getGhcName env
  return $ [ "-outputdir", distDir env ++ "/build/" ++ ghc, "-package-db=" ++ db, "-w", "-no-link" ] ++
           map ("-i" ++) srcDirs ++
           map ("-X" ++) exts ++
           map ("-package " ++) deps ++
           opts ++ cppOpts

ghcBuildExe :: Env -> Section -> IO ()
ghcBuildExe env (Section _ name flds) = do
  initDB env
  let mainIs  = getFieldString flds "main-is"
      srcDirs = getFieldStrings flds ["."] "hs-source-dirs"
      binGhc  = "/bin/ghc/"
      bin     = distDir env ++ binGhc ++ name
  mkdir env $ distDir env ++ binGhc
  mainIs' <- findMainIs env srcDirs mainIs
  stdArgs <- setupStdArgs env flds
  let args    = unwords $ stdArgs ++ ["-o", bin, "--make", mainIs']
  when (verbose env >= 0) $
    putStrLn $ "Build executable " ++ bin ++ " with ghc"
  cmd env $ "ghc " ++ args

findMainIs :: Env -> [FilePath] -> FilePath -> IO FilePath
findMainIs _ [] fn = error $ "cannot find " ++ show fn
findMainIs env (d:ds) fn = do
  let fn' = d ++ "/" ++ fn
  b <- doesFileExist fn'
  if b then
    return fn'
   else
    findMainIs env ds fn

ghcBuildLib :: Env -> Section -> IO ()
ghcBuildLib env (Section _ name flds) = do
  initDB env
  stdArgs <- setupStdArgs env flds
  let mdls = getFieldStrings flds (error "no exposed-modules") "exposed-modules"
      args = unwords $ stdArgs ++ ["--make"] ++ mdls
  when (verbose env >= 0) $
    putStrLn $ "Build library " ++ name ++ " with ghc"
  cmd env $ "ghc " ++ args

ghcInstallLib :: Env -> Section -> IO ()
ghcInstallLib env _cbl = do
  initDB env
  undefined
