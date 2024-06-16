module MicroCabal.Main where
import Control.Monad
import Data.List
import Data.Maybe
import Data.Version
import System.Environment
import System.Exit
import System.Directory
import qualified System.Info as I
import Text.Read
import MicroCabal.Backend.GHC
import MicroCabal.Backend.MHS
import MicroCabal.Cabal
import MicroCabal.Env
import MicroCabal.Normalize
import MicroCabal.Parse
import MicroCabal.StackageList
import MicroCabal.Unix

main :: IO ()
main = do
  (env, args) <- decodeCommonArgs =<< setupEnv

  case args of
    [] -> usage
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
  return Env{ cabalDir = cdir, distDir = "dist-mcabal", verbose = 0, backend = mhsBackend }

decodeCommonArgs :: Env -> IO (Env, [String])
decodeCommonArgs env = do
  let loop e ("-v"    : as) = loop e{ verbose = verbose e + 1 } as
      loop e ("-q"    : as) = loop e{ verbose = -1 } as
      loop e ("--ghc" : as) = loop e{ backend = ghcBackend } as
      loop e ("--mhs" : as) = loop e{ backend = mhsBackend } as
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

-- This is a JSON document enumerating all releases.
stackageSourceList :: URL
stackageSourceList = URL "https://www.stackage.org/download/snapshots.json"

snapshotSource :: String
snapshotSource = "https://raw.githubusercontent.com/commercialhaskell/stackage-snapshots/master/" -- lts/22/13.yaml

-- XX This needs improvement
getBestStackage :: Env -> IO URL
getBestStackage env = do
  -- Get source list
  let dir = cabalDir env
      fsnaps = dir </> snapshotsName
  wget env stackageSourceList fsnaps
  file <- readFile fsnaps
  let snaps = parseSnapshots fsnaps file
      snap = snd $ last $
             [(0::Int, error "no lts snapshots found")] ++
             sort [ (l, r) | (lp, r) <- snaps, Just l <- [stripPrefix "lts-" lp >>= readMaybe] ]
      snap' = map (\ c -> if c == '-' || c == '.' then '/' else c) snap
  when (verbose env > 0) $
    putStrLn $ "Picking Stackage snapshot " ++ snap
  return $ URL $ snapshotSource ++ snap' ++ ".yaml"

cmdUpdate :: Env -> [String] -> IO ()
cmdUpdate env [] = do
  when (verbose env >= 0) $
    putStrLn "Retrieving Stackage package list"
  let dir = cabalDir env
      stk = dir </> snapshotName
      fpkgs = dir </> packageListName
  mkdir env dir
  url <- getBestStackage env
  wget env url stk
  file <- readFile stk
  let yml = parseYAML stk file
      pkgs = yamlToStackageList yml
--  putStrLn $ showYAML yml
--  putStrLn $ show pkgs
  when (verbose env > 0) $
    putStrLn $ "Write package list to " ++ fpkgs
  writeFile fpkgs $ unlines $ map showPackage $ pkgs ++ distPkgs
cmdUpdate _ _ = usage

-- These packages are part of the ghc distribution, so they are
-- not in the stackage list.
-- XXX What to do about versions?
-- XXX more...
distPkgs :: [StackagePackage]
distPkgs =
  [ StackagePackage "containers" (makeVersion [0,6,8]) False []
  , StackagePackage "mtl"        (makeVersion [2,3,1]) False []
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
    when (verbose env >= 0) $
      putStrLn "No package list, running 'update' command"
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
    when (verbose env > 0) $
      putStrLn $ "Already in " ++ pdir
   else do
    mkdir env pdir
    when (verbose env > 0) $
      putStrLn $ "Fetching  " ++ pkgz
    wget env url file
    when (verbose env > 0) $
      putStrLn $ "Unpacking " ++ pkgz ++ " in " ++ pdir
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
  st <- getPackageInfo env pkg
  let dir = dirForPackage env st
  b <- doesDirectoryExist dir
  when (not b) $ do
    when (verbose env >= 0) $
      putStrLn $ "Package not found, running 'fetch " ++ pkg ++ "'"
    cmdFetch env [pkg]
  when (verbose env >= 0) $
    putStrLn $ "Building in " ++ dir
  setCurrentDirectory dir
  cmdBuild env []
cmdBuild _ _ = usage

getGlobal :: Cabal -> Section
getGlobal (Cabal sects) =
  fromMaybe (error "no global section") $ listToMaybe [ s | s@(Section "global" _ _) <- sects ]

build :: Env -> IO ()
build env = do
  fn <- findCabalFile env
  rfile <- readFile fn
  comp <- backendNameVers (backend env) env
  let cbl = parseCabal fn rfile
      info = FlagInfo { os = I.os, arch = I.arch, flags = [], impl = comp }
      ncbl@(Cabal sects) = normalize info cbl
      glob = getGlobal ncbl
      sect s@(Section "executable" _ _) = buildExe env glob s
      sect s@(Section "library"    _ _) = buildLib env glob s
      sect _ = return ()
  mapM_ sect sects

buildExe :: Env -> Section -> Section -> IO ()
buildExe env glob sect@(Section _ name flds) = do
  putStrLn $ "Building executable " ++ name
  let deps = getBuildDepends flds
      pkgs = [ p | (p, _, _) <- deps ]
  mapM_ (checkDep env) pkgs
  buildPkgExe (backend env) env glob sect

buildLib :: Env -> Section -> Section -> IO ()
buildLib env glob sect@(Section _ name flds) = do
  putStrLn $ "Building library " ++ name
  let pkgs = getBuildDependsPkg flds
  mapM_ (checkDep env) pkgs
  buildPkgLib (backend env) env glob sect

checkDep :: Env -> PackageName -> IO ()
checkDep _env pkg | pkg `elem` builtinPackages = return ()
checkDep env pkg = do
  let bend = backend env
  b <- doesPkgExist bend env pkg
  when (not b) $
    error $ "dependency not installed: " ++ pkg

-- XXX These packages are part of mhs.
builtinPackages :: [String]
builtinPackages = ["array", "base", "deepseq", "directory", "process", "bytestring", "text", "fail", "time"]

-----------------------------------------

cmdInstall :: Env -> [String] -> IO ()
cmdInstall env args = do
  -- The will build and change current directory
  cmdBuild env args
  install env

install :: Env -> IO ()
install env = do
  fn <- findCabalFile env
  rfile <- readFile fn
  comp <- backendNameVers (backend env) env
  let cbl = parseCabal fn rfile
      info = FlagInfo { os = I.os, arch = I.arch, flags = [], impl = comp }
      ncbl@(Cabal sects) = normalize info cbl
      glob = getGlobal ncbl
      sect s@(Section "executable" _ _) = installExe env glob s
      sect s@(Section "library"    _ _) = installLib env glob s
      sect _ = return ()
  mapM_ sect sects

installExe :: Env -> Section -> Section -> IO ()
installExe env glob sect@(Section _ name _) = do
  putStrLn $ "Installing executable " ++ name
  installPkgExe (backend env) env glob sect

installLib :: Env -> Section -> Section -> IO ()
installLib env glob sect@(Section _ name _) = do
  putStrLn $ "Installing library " ++ name
  installPkgLib (backend env) env glob sect

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
  \  -v                            be more verbose (can be repeated)\n\
  \  -q                            be quiet\n\
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
  comp <- backendNameVers (backend env) env
  let cbl = parseCabal fn rfile
      info = FlagInfo { os = I.os, arch = I.arch, flags = [], impl = comp }
      ncbl = normalize info cbl
  --putStrLn $ showCabal cbl
  putStrLn $ showCabal ncbl
cmdParse _ _ = undefined
