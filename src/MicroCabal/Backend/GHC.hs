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

-- XXX needs version info
getGhcDir :: Env -> IO FilePath
getGhcDir env = return $ cabalDir env ++ "/ghc"

initDB :: Env -> IO ()
initDB env = do
  dir <- getGhcDir env
  b <- doesDirectoryExist dir
  when (not b) $ do
    cmd env $ "ghc-pkg init " ++ dir

ghcExists :: Env -> PackageName -> IO Bool
ghcExists env pkgname = do
  dir <- getGhcDir env
  tryCmd env $ "ghc-pkg --package-db=" ++ dir ++ " describe >/dev/null 2>/dev/null " ++ pkgname

ghcBuildExe :: Env -> Section -> IO ()
ghcBuildExe env (Section _ name flds) = do
  initDB env
  db <- getGhcDir env
  let mainIs  = getFieldString  flds "main-is"
      srcDirs = getFieldStrings flds "hs-source-dirs"
      exts    = getFieldStrings flds "default-extensions"
      opts    = getFieldStrings flds "ghc-options"
      bin     = distDir env ++ "/bin/" ++ name
  mkdir env $ distDir env ++ "/bin"
  mainIs' <- findMainIs env srcDirs mainIs
  let args    = unwords $ [ "-outputdir", distDir env ++ "/build", "-package-db=" ++ db ] ++
                          map ("-i" ++) srcDirs ++
                          map ("-X" ++) exts ++
                          opts ++
                          ["-o", bin, "--make", mainIs']
  when (verbose env >= 0) $
    putStrLn $ "Build " ++ bin ++ " with ghc"
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
ghcBuildLib env _cbl = do
  initDB env
  undefined

ghcInstallLib :: Env -> Section -> IO ()
ghcInstallLib env _cbl = do
  initDB env
  undefined
