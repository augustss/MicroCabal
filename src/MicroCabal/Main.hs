module MicroCabal.Main where
import Control.Monad
import Data.List
import Data.Maybe
import Data.Version
import System.Environment
import System.Exit
import System.Directory
import qualified System.Info as I
--import Text.Read
import MicroCabal.Backend.GHC
import MicroCabal.Backend.MHS
import MicroCabal.Cabal
import MicroCabal.Env
import MicroCabal.Glob
import MicroCabal.Normalize
import MicroCabal.Parse
import MicroCabal.StackageList
import MicroCabal.Unix
--import MicroCabal.YAML

version :: String
version = "MicroCabal 0.2.1.0"

main :: IO ()
main = do
  (env, args) <- decodeCommonArgs =<< setupEnv

  case args of
    [] -> usage
    ["--version"]  -> putStrLn version
    "build"   : as -> cmdBuild   env as
    "clean"   : as -> cmdClean   env as
    "fetch"   : as -> cmdFetch   env as
    "help"    : as -> cmdHelp    env as
    "install" : as -> cmdInstall env as
    "parse"   : as -> cmdParse   env as
    "update"  : as -> cmdUpdate  env as
    _ -> usage

setupEnv :: IO Env
setupEnv = do
  home <- getEnv "HOME"
  let cdir = home </> ".mcabal"
      env = Env{ cabalDir = cdir, distDir = "dist-mcabal", verbose = 0, depth = 0,
                 backend = undefined, recursive = False, targets = [TgtLib, TgtExe] }
  be <- mhsBackend env
  return env{ backend = be }

decodeCommonArgs :: Env -> IO (Env, [String])
decodeCommonArgs env = do
  let loop e ("-v"    : as) = loop e{ verbose = verbose e + 1 } as
      loop e ("-q"    : as) = loop e{ verbose = -1 } as
      loop e ("-r"    : as) = loop e{ recursive = True } as
      loop e ("--ghc" : as) = do be <- ghcBackend env; loop e{ backend = be } as
      loop e ("--mhs" : as) = do be <- mhsBackend env; loop e{ backend = be } as
      loop e as = return (e, as)
  loop env =<< getArgs

usage :: IO ()
usage = do
  env <- setupEnv
  cmdHelp env []
  exitWith (ExitFailure 1)

-----------------------------------------

-- Package list
packageListName :: FilePath
packageListName = "packages.txt"

-- Local name for snapshot list
snapshotsName :: FilePath
snapshotsName = "snapshots.json"

-- Local name for snapshot
snapshotName :: FilePath
snapshotName = "snapshot.yaml"

-- Name of the nightly snapshot
nightlyName :: String
nightlyName = "nightly"

-- This is a JSON document enumerating all releases.
stackageSourceList :: URL
stackageSourceList = URL "https://stackage-haddock.haskell.org/snapshots.json"

-- prefix of URL for actual snapshot
snapshotSource :: String
snapshotSource = "https://raw.githubusercontent.com/commercialhaskell/stackage-snapshots/master/" -- lts/22/13.yaml

-- XXX This needs improvement
getBestStackage :: Env -> IO URL
getBestStackage env = do
  -- Get source list
  let dir = cabalDir env
      fsnaps = dir </> snapshotsName
  wget env stackageSourceList fsnaps
  file <- readFile fsnaps
  let snaps = parseSnapshots fsnaps file
{-
      -- Pick LTS snapshot
      snap = snd $ last $
             [(0::Int, error "no lts snapshots found")] ++
             sort [ (l, r) | (lp, r) <- snaps, Just l <- [stripPrefix "lts-" lp >>= readMaybe] ]
      snap' = map (\ c -> if c == '-' || c == '.' then '/' else c) snap
      snapURL = URL $ snapshotSource ++ snap' ++ ".yaml"
-}
      -- Pick a nightly snapshot
      snap = fromMaybe (error "no nightly snapshot found") $ lookup nightlyName snaps
      snap' = fixLeading0 $ map (\ c -> if c == '-' then '/' else c) snap
      fixLeading0 ('/':'0':cs) = '/' : fixLeading0 cs
      fixLeading0 (c:cs) = c : fixLeading0 cs
      fixLeading0 cs = cs
      snapURL = URL $ snapshotSource ++ snap' ++ ".yaml"
  message env 1 $ "Picking Stackage snapshot " ++ snap
  return $ snapURL

cmdUpdate :: Env -> [String] -> IO ()
cmdUpdate env [] = do
  message env 0 "Retrieving Stackage package list"
  let dir = cabalDir env
      stk = dir </> snapshotName
      fpkgs = dir </> packageListName
  mkdir env dir
  url <- getBestStackage env
  wget env url stk
  file <- readFile stk
  let yml = parseYAML stk file
      pkgs = yamlToStackageList yml
      ghcVersion = yamlToGHCVersion yml
--  putStrLn $ "==== " ++ ghcVersion
--  putStrLn $ showYAML yml
--  putStrLn $ show pkgs
  message env 1 $ "Write package list to " ++ fpkgs
  writeFile fpkgs $ unlines $ map showPackage $ pkgs ++ distPkgs
  writeFile (dir </> "ghc-version") ghcVersion
cmdUpdate _ _ = usage

-- These packages are part of the ghc distribution, so they are
-- not in the stackage list.
-- XXX What to do about versions?
-- XXX more...
distPkgs :: [StackagePackage]
distPkgs =
  [ StackagePackage "containers" (makeVersion [0,6,8])    False []
  , StackagePackage "deepseq"    (makeVersion [1,5,0,0])  False []
  , StackagePackage "mtl"        (makeVersion [2,3,1])    False []
  , StackagePackage "time"       (makeVersion [1,12,2])   False []
  ]

-----------------------------------------

hackageSrcURL :: String
hackageSrcURL = "https://hackage.haskell.org/package/"

getPackageList :: Env -> IO [StackagePackage]
getPackageList env = do
  let dir = cabalDir env
      fpkgs = dir </> packageListName
  b <- doesFileExist fpkgs
  when (not b) $ do
    message env 0 "No package list, running 'update' command"
    cmdUpdate env []
  map readPackage . lines <$> readFile fpkgs

getPackageInfo :: Env -> PackageName -> IO StackagePackage
getPackageInfo env pkg = do
  pkgs <- getPackageList env
  return $ fromMaybe (error $ "getPackageInfo: no package " ++ pkg) $ listToMaybe $ filter ((== pkg) . stName) pkgs

dirPackage :: Env -> FilePath
dirPackage env = cabalDir env </> "packages"

dirForPackage :: Env -> StackagePackage -> FilePath
dirForPackage env st = dirPackage env </> stName st ++ "-" ++ showVersion (stVersion st)

cmdFetch :: Env -> [String] -> IO ()
cmdFetch env [pkg] = do
  st <- getPackageInfo env pkg
  let pkgs = stName st ++ "-" ++ showVersion (stVersion st)
      url  = URL $ hackageSrcURL ++ pkgs </> pkgz
      pkgz = pkgs ++ ".tar.gz"
      pdir = dirForPackage env st
      file = pdir ++ ".tar.gz"
  b <- doesDirectoryExist pdir
  if b then
    message env 1 $ "Already in " ++ pdir
   else do
    mkdir env pdir
    message env 1 $ "Fetching  " ++ pkgz
    wget env url file
    message env 1 $ "Unpacking " ++ pkgz ++ " in " ++ pdir
    tarx env (dirPackage env) file
cmdFetch _ _ = usage

-----------------------------------------

findCabalFile :: Env -> IO FilePath
findCabalFile _env = do
  ns <- listDirectory "."
  case filter (".cabal" `isSuffixOf`) ns of
    []  -> error "no PKG.cabal file"
    [n] -> return n
    _   -> error "multiple PKG.cabal file"

cmdBuild :: Env -> [String] -> IO ()
cmdBuild env [] = build env
cmdBuild env [pkg] = do
  message env 0 $ "Build package " ++ pkg
  st <- getPackageInfo env pkg
  let dir = dirForPackage env st
  b <- doesDirectoryExist dir
  when (not b) $ do
    message env 0 $ "Package not found, running 'fetch " ++ pkg ++ "'"
    cmdFetch env [pkg]
  message env 0 $ "Building in " ++ dir
  setCurrentDirectory dir
  cmdBuild env []
cmdBuild _ _ = usage

getGlobal :: Cabal -> Section
getGlobal (Cabal sects) =
  fromMaybe (error "no global section") $ listToMaybe [ s | s@(Section "global" _ _) <- sects ]

createPathFile :: Env -> Section -> Section -> IO ()
createPathFile env (Section _ _ glob) (Section _ name _) = do
  let mdlName = "Paths_" ++ map (\ c -> if c == '-' then '_' else c) name
      pathName = pathModuleDir env </> mdlName ++ ".hs"
      vers = getVersion glob "version"
--      dataDir = "???" -- cabalDir env </> "COMPILER-VERSION" </> "data" </> pkgVers </> "data"
  message env 1 $ "Creating path module " ++ pathName
  mkdir env (pathModuleDir env)
  writeFile pathName $
    "module " ++ mdlName ++ " where\n" ++
    "import Data.Version\n" ++
    "version :: Version; version = make" ++ show vers ++ "\n"
--    ++ "getDataDir :: IO FilePath; getDataDir = return " ++ show dataDir ++ "\n"

build :: Env -> IO ()
build env = do
  fn <- findCabalFile env
  rfile <- readFile fn
  let comp = backendNameVers (backend env)
  let cbl = parseCabal fn rfile
      info = FlagInfo { os = I.os, arch = I.arch, flags = [], impl = comp }
      ncbl@(Cabal sects) = normalize info cbl
      glob = getGlobal ncbl
      sect s@(Section "executable" _ _) | TgtExe `elem` targets env = buildExe env glob s
      sect s@(Section "library"    _ _) | TgtLib `elem` targets env = buildLib env glob s
      sect _ = return ()
  mapM_ sect $ addMissing sects

buildExe :: Env -> Section -> Section -> IO ()
buildExe env glob sect@(Section _ name flds) = do
  message env 0 $ "Building executable " ++ name
  createPathFile env glob sect
  let deps = getBuildDepends flds
      pkgs = [ p | (p, _, _) <- deps ]
  mapM_ (checkDep env) pkgs
  buildPkgExe (backend env) env glob sect

buildLib :: Env -> Section -> Section -> IO ()
buildLib env glob sect@(Section _ name flds) = do
  message env 0 $ "Building library " ++ name
  createPathFile env glob sect
  let pkgs = getBuildDependsPkg flds
  mapM_ (checkDep env) pkgs
  buildPkgLib (backend env) env glob sect

checkDep :: Env -> PackageName -> IO ()
checkDep env pkg = do
  let bend = backend env
  b <- doesPkgExist bend env pkg
  when (not b) $
    if recursive env then do
      let env' = env { depth = depth env + 1 }
      preserveCurrentDirectory $
        cmdInstallLib env' [pkg]
    else
      error $ "dependency not installed: " ++ pkg

-- If there is no section, except the global one, then just make a
-- library section.
addMissing :: [Section] -> [Section]
addMissing [glb@(Section "global" _ flds)] = [glb, Section "library" (getFieldString flds "name") flds]
addMissing sects = sects

-----------------------------------------

cmdInstall :: Env -> [String] -> IO ()
cmdInstall env args = do
  -- The will build and change current directory
  cmdBuild env args
  install env

cmdInstallLib :: Env -> [String] -> IO ()
cmdInstallLib env args = cmdInstall env{ targets = [TgtLib] } args

install :: Env -> IO ()
install env = do
  fn <- findCabalFile env
  rfile <- readFile fn
  let comp = backendNameVers (backend env)
  let cbl = parseCabal fn rfile
      info = FlagInfo { os = I.os, arch = I.arch, flags = [], impl = comp }
      ncbl@(Cabal sects) = normalize info cbl
      glob = getGlobal ncbl
      sect s@(Section "executable" _ _) | TgtExe `elem` targets env = installExe env glob s
      sect s@(Section "library"    _ _) | TgtLib `elem` targets env = installLib env glob s
      sect _ = return ()
  mapM_ sect $ addMissing sects

installExe :: Env -> Section -> Section -> IO ()
installExe env glob sect@(Section _ name _) = do
  message env 0 $ "Installing executable " ++ name
  installDataFiles env glob sect
  installPkgExe (backend env) env glob sect

installLib :: Env -> Section -> Section -> IO ()
installLib env glob sect@(Section _ name _) = do
  message env 0 $ "Installing library " ++ name
  installDataFiles env glob sect
  installPkgLib (backend env) env glob sect

installDataFiles :: Env -> Section -> Section -> IO ()
installDataFiles env _glob _sect@(Section _ _ flds) = do
  case getFieldStrings flds [] "data-files" of
    [] -> return ()
    pats -> do
      files <- matchFiles "." pats
      message env 1 $ "Installing data files " ++ unwords files
      let tgt = undefined
      copyFiles env "." files tgt

-----------------------------------------

cmdHelp :: Env -> [String] -> IO ()
cmdHelp _ _ = putStrLn "\
  \Available commands:\n\
  \  mcabal [FLAGS] build [PKG]    build in current directory, or the package PKG\n\
  \  mcabal [FLAGS] clean          clean in the current directory\n\
  \  mcabal [FLAGS] fetch PKG      fetch files for package PKG\n\
  \  mcabal [FLAGS] help           show this message\n\
  \  mcabal [FLAGS] install        build and install in current directory\n\
  \  mcabal [FLAGS] parse FILE     just parse a Cabal file (for debugging)\n\
  \  mcabal [FLAGS] update         retrieve new set of consistent packages\n\
  \\n\
  \Flags:\n\
  \  --version                     show version\n\
  \  -v                            be more verbose (can be repeated)\n\
  \  -q                            be quiet\n\
  \  -r                            do recursive installs for missing packages\n\
  \  --ghc                         compile using ghc\n\
  \  --mhs                         compile using mhs (default)\n\
  \\n\
  \"

-----------------------------------------

cmdClean :: Env -> [String] -> IO ()
cmdClean env _ = rmrf env (distDir env)

-----------------------------------------

cmdParse :: Env -> [String] -> IO ()
cmdParse env [fn] = do
  rfile <- readFile fn
  let comp = backendNameVers (backend env)
  let cbl = parseCabal fn rfile
      info = FlagInfo { os = I.os, arch = I.arch, flags = [], impl = comp }
      ncbl = normalize info cbl
  --putStrLn $ showCabal cbl
  putStrLn $ showCabal ncbl
cmdParse _ _ = undefined
