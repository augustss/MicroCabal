module MicroCabal.Backend.MHS(mhsBackend) where
import Control.Monad
import System.Directory
import MicroCabal.Cabal
import MicroCabal.Env
import MicroCabal.Unix

mhsBackend :: Backend
mhsBackend = Backend {
  backendName = "mhs",
  doesPkgExist = mhsExists,
  buildPkgExe = mhsBuildExe,
  buildPkgLib = mhsBuildLib,
  installPkgExe = mhsInstallExe,
  installPkgLib = mhsInstallLib
  }

-- XXX needs version info
getMhsDir :: Env -> IO FilePath
getMhsDir env = return $ cabalDir env ++ "/mhs"

initDB :: Env -> IO ()
initDB env = do
  dir <- getMhsDir env
  b <- doesDirectoryExist dir
  when (not b) $ do
    --cmd env $ "mhs-pkg init " ++ dir
    mkdir env dir

mhsExists :: Env -> PackageName -> IO Bool
mhsExists env _pkgname = do
  _dir <- getMhsDir env
  return False

setupStdArgs :: Env -> [Field] -> [String]
setupStdArgs _env flds =
  let srcDirs = getFieldStrings flds ["."]   "hs-source-dirs"
      defExts = getFieldStrings flds []      "default-extensions"
      exts    = getFieldStrings flds defExts "extensions"
      opts    = getFieldStrings flds []      "mhs-options"
      cppOpts = getFieldStrings flds []      "cpp-options"
      exts'   = filter (`elem` mhsX) exts
      mhsX    = ["CPP"]
  in  map ("-i" ++) srcDirs ++
      map ("-X" ++) exts' ++
      opts ++ cppOpts

binMhs :: String
binMhs  = "/bin/ghc/"

mhsBuildExe :: Env -> Section -> Section -> IO ()
mhsBuildExe env _ (Section _ name flds) = do
  initDB env
  let mainIs  = getFieldString  flds         "main-is"
      srcDirs = getFieldStrings flds ["."]   "hs-source-dirs"
      bin     = distDir env ++ binMhs ++ name
  mkdir env $ distDir env ++ binMhs
  mainIs' <- findMainIs env srcDirs mainIs
  let args    = unwords $ setupStdArgs env flds ++
                          ["-o" ++ bin, mainIs']
  when (verbose env >= 0) $
    putStrLn $ "Build " ++ bin ++ " with mhs"
  --putStrLn $ "mhs " ++ args
  cmd env $ "MHSDIR=/usr/local/lib/mhs " ++    -- temporary hack
            "mhs " ++ args

findMainIs :: Env -> [FilePath] -> FilePath -> IO FilePath
findMainIs _ [] fn = error $ "cannot find " ++ show fn
findMainIs env (d:ds) fn = do
  let fn' = d ++ "/" ++ fn
  b <- doesFileExist fn'
  if b then
    return fn'
   else
    findMainIs env ds fn

mhsBuildLib :: Env -> Section -> Section -> IO ()
mhsBuildLib env _ (Section _ name flds) = do
  initDB env
  let mdls = getFieldStrings flds (error "no exposed-modules") "exposed-modules"
      args = unwords $ ["-P" ++ name] ++
                       setupStdArgs env flds ++ mdls
  error $ "No mhsBuildLib\n" ++ show args

mhsInstallExe :: Env -> Section -> Section -> IO ()
mhsInstallExe env (Section _ _ _glob) (Section _ name _) = do
  let bin = distDir env ++ binMhs ++ name
      binDir = cabalDir env ++ "/bin"
  cp env bin binDir

mhsInstallLib :: Env -> Section -> Section -> IO ()
mhsInstallLib env _glob _sect = do
  initDB env
  undefined
