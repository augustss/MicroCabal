module MicroCabal.Main where
import Control.Monad
import Data.List
import Data.Maybe
import Data.Version
import System.Environment
import System.Directory
import qualified System.Info as I
import Text.Read
import MicroCabal.Backend.GHC
import MicroCabal.Cabal
import MicroCabal.Env
import MicroCabal.Normalize
import MicroCabal.Parse
import MicroCabal.StackageList
import MicroCabal.Unix
import MicroCabal.YAML

main :: IO ()
main = do
  (env, args) <- decodeCommonArgs =<< setupEnv

  case args of
    [] -> usage
    "build"   : as -> cmdBuild   env as
    "fetch"   : as -> cmdFetch   env as
    "help"    : as -> cmdHelp    env as
    "install" : as -> cmdInstall env as
    "upgrade" : as -> cmdUpgrade env as
    "test"    : as -> cmdTest    env as
    _ -> usage

{-
  let fn = args !! 0
  rfile <- readFile fn
  if False then do
    let cbl = parseCabal fn rfile
        info = FlagInfo { os = I.os, arch = I.arch, flags = [], impl = (I.compilerName, I.compilerVersion) }
        ncbl = normalize info cbl
    putStrLn $ showCabal cbl
    putStrLn $ showCabal ncbl
   else do
    let y = parseYAML fn rfile
    putStrLn $ showYAML y
    putStrLn $ show $ yamlToStackageList y
-}

setupEnv :: IO Env
setupEnv = do
  home <- getEnv "HOME"
  let cdir = home ++ "/.mcabal"
  return Env{ cabalDir = cdir, distDir = "dist-mcabal", verbose = 0, backend = ghcBackend }

decodeCommonArgs :: Env -> IO (Env, [String])
decodeCommonArgs env = do
  let loop e ("-v" : as) = loop e{ verbose = verbose e + 1 } as
      loop e ("-q" : as) = loop e{ verbose = -1 } as
      loop e as = return (e, as)
  loop env =<< getArgs

usage :: IO ()
usage = do
  putStrLn "\
\Usage:\n\
\  mcabal [-v] build [PKG]\n\
\  mcabal [-v] fetch PKG\n\
\  mcabal [-v] install\n\
\  mcabal [-v] upgrade\n\
\"
  error "done"

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

-- This is a JSON document describing enumerating all releases.
stackageSourceList :: URL
stackageSourceList = URL "https://www.stackage.org/download/snapshots.json"

snapshotSource :: String
snapshotSource = "https://raw.githubusercontent.com/commercialhaskell/stackage-snapshots/master/" -- lts/22/13.yaml

-- XX This needs improvement
getBestStackage :: Env -> IO URL
getBestStackage env = do
  -- Get source list
  let dir = cabalDir env
      fsnaps = dir ++ "/" ++ snapshotsName
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

cmdUpgrade :: Env -> [String] -> IO ()
cmdUpgrade env _args = do
  when (verbose env >= 0) $
    putStrLn "Retrieving Stackage package list"
  let dir = cabalDir env
      stk = dir ++ "/" ++ snapshotName
      fpkgs = dir ++ "/" ++ packageListName
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
  writeFile fpkgs $ unlines $ map showPackage pkgs

cmdTest :: Env -> [String] -> IO ()
cmdTest _ _ = do
  let stk = "11.yaml"
  file <- readFile stk
  let yml = parseYAML stk file
  putStrLn $ showYAML yml


-----------------------------------------

hackageSrcURL :: String
hackageSrcURL = "https://hackage.haskell.org/package/"

getPackageList :: Env -> IO [StackagePackage]
getPackageList env = do
  let dir = cabalDir env
      fpkgs = dir ++ "/" ++ packageListName
  b <- doesFileExist fpkgs
  when (not b) $ do
    when (verbose env >= 0) $
      putStrLn "No package list, running 'upgrade' command"
    cmdUpgrade env []
  map readPackage . lines <$> readFile fpkgs

getPackageInfo :: Env -> PackageName -> IO StackagePackage
getPackageInfo env pkg = do
  pkgs <- getPackageList env
  return $ fromMaybe (error $ "getPackageInfo: no package " ++ pkg) $ listToMaybe $ filter ((== pkg) . stName) pkgs

dirForPackage :: Env -> String -> FilePath
dirForPackage env pkg = cabalDir env ++ "/packages/" ++ pkg

cmdFetch :: Env -> [String] -> IO ()
cmdFetch env [pkg] = do
  st <- getPackageInfo env pkg
  let pkgs = stName st ++ "-" ++ showVersion (stVersion st)
      url  = URL $ hackageSrcURL ++ pkgs ++ "/" ++ pkgz
      pkgz = pkgs ++ ".tar.gz"
      dir  = dirForPackage env pkg
      file = dir ++ ".tar.gz"
  b <- doesDirectoryExist dir
  when (not b) $ do
    mkdir env dir
    when (verbose env > 0) $
      putStrLn $ "Fetching  " ++ pkgz
    wget env url file
    when (verbose env > 0) $
      putStrLn $ "Unpacking " ++ pkgz
    tarx env dir file
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
  let dir = dirForPackage env pkg
  b <- doesDirectoryExist dir
  when (not b) $ do
    when (verbose env >= 0) $
      putStrLn $ "Package not found, running 'fetch " ++ pkg ++ "'"
    cmdFetch env [pkg]
  setCurrentDirectory dir
  cmdBuild env []
cmdBuild _ _ = usage

build :: Env -> IO ()
build env = do
  fn <- findCabalFile env
  rfile <- readFile fn
  let cbl = parseCabal fn rfile
      info = FlagInfo { os = I.os, arch = I.arch, flags = [], impl = (I.compilerName, I.compilerVersion) }
      Cabal sects = normalize info cbl
      sect s@(Section "executable" _ _) = buildExe env s
      sect s@(Section "library"    _ _) = buildLib env s
      sect _ = return ()
  mapM_ sect sects

buildExe :: Env -> Section -> IO ()
buildExe env sect@(Section _ _ flds) = do
  let deps = getBuildDepends flds
      pkgs = [ p | (p, _, _) <- deps ]
  mapM_ (checkDep env) pkgs
  buildPkgExe (backend env) env sect

buildLib :: Env -> Section -> IO ()
buildLib env sect@(Section _ _ flds) = do
  let deps = getBuildDepends flds
      pkgs = [ p | (p, _, _) <- deps ]
  mapM_ (checkDep env) pkgs
  buildPkgLib (backend env) env sect

getBuildDepends :: [Field] -> [(Item, [Item], Maybe VersionRange)]
getBuildDepends fs =
  case [ d | Field "build-depends" (VPkgs d) <- fs ] of
    [d] -> d
    _   -> []

checkDep :: Env -> PackageName -> IO ()
checkDep _env pkg | pkg `elem` ["base", "directory", "process"] = return ()
checkDep env pkg = do
  let bend = backend env
  b <- doesPkgExist bend env pkg
  when (not b) $
    error $ "dependency not installed: " ++ pkg

-----------------------------------------

cmdInstall :: Env -> [String] -> IO ()
cmdInstall _env _args = undefined

-----------------------------------------

cmdHelp :: Env -> [String] -> IO ()
cmdHelp _ _ = putStrLn "Coming soon"
