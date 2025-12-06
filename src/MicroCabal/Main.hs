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
import MicroCabal.Glob
import MicroCabal.Normalize
import MicroCabal.Parse
import MicroCabal.StackageList
import MicroCabal.Unix
--import MicroCabal.YAML

version :: String
version = "MicroCabal 0.5.4.0"

main :: IO ()
main = do
  (env, args) <- decodeCommonArgs =<< setupEnv
  case args of
    [] -> usage
    ["--version"]  -> putStrLn version
    "build"   : as -> decodeGit cmdBuild   env as
    "clean"   : as ->           cmdClean   env as
    "fetch"   : as -> decodeGit cmdFetch   env as
    "help"    : as ->           cmdHelp    env as
    "install" : as -> decodeGit cmdInstall env as
    "parse"   : as ->           cmdParse   env as
    "update"  : as ->           cmdUpdate  env as
    _ -> usage

setupEnv :: IO Env
setupEnv = do
  cdirm <- lookupEnv "CABALDIR"
  home <- getEnv "HOME"
  let cdir = fromMaybe (home </> ".mcabal") cdirm
      env = Env{ cabalDir = cdir, distDir = "dist-mcabal", verbose = 0, depth = 0, eflags = [],
                 backend = error "backend undefined", recursive = False, targets = [TgtLib, TgtFor, TgtExe],
                 gitRepo = Nothing, dryRun = False, useNightly = True }
  be <- mhsBackend env
  return env{ backend = be }

decodeCommonArgs :: Env -> IO (Env, [String])
decodeCommonArgs env = do
  let loop e ("-v"           : as) = loop e{ verbose = verbose e + 1 } as
      loop e ("-q"           : as) = loop e{ verbose = -1 } as
      loop e ("-r"           : as) = loop e{ recursive = True } as
      loop e (('-':'f':s)    : as) = loop e{ eflags = decodeCabalFlags s } as
      loop e ("--ghc"        : as) = do be <- ghcBackend env; loop e{ backend = be } as
      loop e ("--mhs"        : as) = do be <- mhsBackend env; loop e{ backend = be } as
      loop e ("--dry-run"    : as) = loop e{ dryRun = True } as
      loop e ("--nightly"    : as) = loop e{ useNightly = True } as
      loop e ("--no-nightly" : as) = loop e{ useNightly = False } as
      loop e as = return (e, as)
  loop env =<< getArgs

decodeCabalFlags :: String -> [(Name, Bool)]
decodeCabalFlags = map f . words
  where f ('-':s) = (s, False)
        f s       = (s, True)

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
      (snap, snap') =
        if useNightly env then
          -- Pick nightly snapshot
          let snap = fromMaybe (error "no nightly snapshot found") $ lookup nightlyName snaps
              fixLeading0 ('/':'0':cs) = '/' : fixLeading0 cs
              fixLeading0 (c:cs) = c : fixLeading0 cs
              fixLeading0 cs = cs
          in  (snap, fixLeading0 $ map (\ c -> if c == '-' then '/' else c) snap)
        else
          -- Pick LTS snapshot
          let snap = snd $ last $
                     [(0::Int, error "no lts snapshots found")] ++
                     sort [ (l, r) | (lp, r) <- snaps, Just l <- [stripPrefix "lts-" lp >>= readMaybe] ]
          in  (snap, map (\ c -> if c == '-' || c == '.' then '/' else c) snap)

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
  dist <- getDistPkgs
  let yml = parseYAML stk file
      pkgs = map hackName $ yamlToStackageList yml ++ dist
      ghcVersion = yamlToGHCVersion yml
      hackName s = s{ stName = n, stVersion = v } where (n, v) = patchName (backend env) (stName s, stVersion s)
--  putStrLn $ "==== " ++ ghcVersion
--  putStrLn $ showYAML yml
--  putStrLn $ show pkgs
  message env 1 $ "Write package list to " ++ fpkgs
  writeFile fpkgs $ unlines $ map showPackage pkgs
  writeFile (dir </> "ghc-version") ghcVersion
cmdUpdate _ _ = usage

-- These packages are part of the ghc distribution, so they are
-- not in the stackage list.
-- XXX What to do about versions?
-- XXX more...
-- Should get these from global-hints (?)
-- https://raw.githubusercontent.com/commercialhaskell/stackage-content/master/stack/global-hints.yaml
getDistPkgs :: IO [StackagePackage]
getDistPkgs = return distPkgs

distPkgs :: [StackagePackage]
distPkgs =
  [ StackagePackage "array"        (makeVersion [0,5,8,0])    False []
  , StackagePackage "containers"   (makeVersion [0,8])        False []
--  , StackagePackage "deepseq"      (makeVersion [1,6,0,0])  False []  -- built in
  , StackagePackage "exceptions"   (makeVersion [0,10,9])     False []
  , StackagePackage "filepath"     (makeVersion [1,5,4,0])    False []
  , StackagePackage "ghc-compat"   (makeVersion [0,5,1,0])    False []
  , StackagePackage "mtl"          (makeVersion [2,3,1])      False []
  , StackagePackage "os-string"    (makeVersion [2,0,7])      False []
  , StackagePackage "parsec"       (makeVersion [3,1,18,0])   False []
  , StackagePackage "pretty"       (makeVersion [1,1,3,6])    False []
  , StackagePackage "time"         (makeVersion [1,15])       False []
  , StackagePackage "transformers" (makeVersion [0,6,2,0])    False []
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
    message env 1 $ "Fetching package " ++ pkgs
    case gitRepo env of
      Nothing -> do
        message env 1 $ "Fetching from Hackage " ++ pkgz
        wget env url file
        message env 1 $ "Unpacking " ++ pkgz ++ " in " ++ pdir
        tarx env (dirPackage env) file
      Just repo -> do
        message env 1 $ "Fetching from git repo " ++ pkg
        gitClone env pdir (URL repo)
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
cmdBuild env [apkg] = do
  let pkg = fst $ patchName (backend env) (apkg, undefined)
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

makeDataPrefix :: Env -> Section -> Section -> FilePath
makeDataPrefix env (Section _ _ glob) (Section _ name _) =
  let vers = getVersion glob "version"
      pkgVers = name ++ "-" ++ showVersion vers
      dataPrefix = cabalDir env </> compiler (backend env) </> "packages" </> pkgVers
  in  dataPrefix

createPathFile :: Env -> Section -> Section -> IO ()
createPathFile env gsect@(Section _ _ glob) sect = do
  let vers = getVersion glob "version"
      name = getFieldString glob "name"
      mdlName = "Paths_" ++ map (\ c -> if c == '-' then '_' else c) name
      pathName = pathModuleDir env </> mdlName ++ ".hs"
      dataPrefix = makeDataPrefix env gsect sect
      dataDir = dataPrefix </> "data"
  message env 1 $ "Creating path module " ++ pathName
  mkdir env (pathModuleDir env)
  writeFile pathName $
    "module " ++ mdlName ++ " where\n" ++
    "import Data.Version\n" ++
    "version :: Version; version = makeVersion " ++ show (versionBranch vers) ++ "\n" ++
    "getDataDir :: IO FilePath; getDataDir = return " ++ show dataDir ++ "\n"

build :: Env -> IO ()
build env = do
  fn <- findCabalFile env
  rfile <- readFile fn
  let comp = backendNameVers (backend env)
  let cbl = parseCabal fn rfile
      info = FlagInfo { os = I.os, arch = I.arch, flags = eflags env, impl = comp }
      ncbl@(Cabal sects) = normalizeAndPatch env info cbl
      glob = getGlobal ncbl
      sectLib s@(Section "library"         _ _) | TgtLib `elem` targets env && isBuildable s = buildLib env glob s
      sectLib s@(Section "foreign-library" _ _) | TgtFor `elem` targets env && isBuildable s = buildForeignLib env glob s
      sectLib _ = return Nothing
      sectExe ll s@(Section "executable"      _ _) | TgtExe `elem` targets env && isBuildable s = buildExe env glob s ll
      sectExe _ _ = return ()
      sects' = addMissing sects
  message env 3 $ "Unnormalized Cabal file:\n" ++ show cbl
  message env 2 $ "Normalized Cabal file:\n" ++ show ncbl
  -- Build libs first, then exes
  localLibs <- mapM sectLib sects'
  mapM_ (sectExe $ catMaybes localLibs) sects'

isBuildable :: Section -> Bool
isBuildable (Section _ _ flds) = getFieldBool True flds "buildable"

buildExe :: Env -> Section -> Section -> [Name] -> IO ()
buildExe env glob@(Section _ _ sglob) sect@(Section _ name flds) localLibs = do
  message env 0 $ "Building executable " ++ name
  createPathFile env glob sect
  let deps = getBuildDepends flds
      pkgs = [ p | (p, _, _) <- deps, p `notElem` localLibs ]
      vers =  getVersion sglob "version"
      sect' = hackLocalLibs env localLibs vers sect
  mapM_ (checkDep env) pkgs
  buildPkgExe (backend env) env glob sect'

-- Remove dependencies on locally built libraries and add them
-- as preloads of the mhs-options.
hackLocalLibs :: Env -> [Name] -> Version -> Section -> Section
hackLocalLibs env locals vers (Section stype sname flds) =
  Section stype sname $ addLocals $ map removeLocals flds
  where removeLocals (Field "build-depends" (VPkgs ps)) =
          Field "build-depends" $ VPkgs $ filter (\ (n, _, _) -> n `notElem` locals) ps
        removeLocals f = f
        addLocals fs =
          let mhso = "mhs-options"
              mfs = getFieldStrings fs [] mhso
              mfs' = VItems (map ("-p" ++) localFiles ++ mfs)
          in  replField mhso mfs' fs
        localFiles = map (\ n -> distDir env ++ "/" ++  n ++ "-" ++ showVersion vers ++ ".pkg") locals

buildLib :: Env -> Section -> Section -> IO (Maybe Name)
buildLib env glob sect@(Section _ name flds) = do
  message env 0 $ "Building library " ++ name
  createPathFile env glob sect
  let pkgs = getBuildDependsPkg flds
  mapM_ (checkDep env) pkgs
  buildPkgLib (backend env) env glob sect
  return (Just name)

buildForeignLib :: Env -> Section -> Section -> IO (Maybe Name)
buildForeignLib env glob sect@(Section _ name flds) = do
  message env 0 $ "Building foreign-library " ++ name
  createPathFile env glob sect
  let pkgs = getBuildDependsPkg flds
  mapM_ (checkDep env) pkgs
  buildPkgForLib (backend env) env glob sect
  return (Just name)

checkDep :: Env -> PackageName -> IO ()
checkDep env pkg = do
  let bend = backend env
  b <- doesPkgExist bend env pkg
  when (not b) $
    if recursive env then do
      let env' = env { depth = depth env + 1 }
      preserveCurrentDirectory $
        cmdInstallLib env' [pkg]
      message env 0 "Return to building target"
    else
      error $ "dependency not installed: " ++ pkg

-- If there is no section, except the global one, then just make a
-- library section.
addMissing :: [Section] -> [Section]
addMissing [glb@(Section "global" _ flds)] = [glb, Section "library" (getFieldString flds "name") flds]
addMissing sects = sects

-----------------------------------------

decodeGit :: (Env -> [String] -> IO ()) -> Env -> [String] -> IO ()
decodeGit io env (arg:args) | repo@(Just _) <- stripPrefix "--git=" arg = io (env{ gitRepo = repo }) args
decodeGit io env args = io env args

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
      info = FlagInfo { os = I.os, arch = I.arch, flags = eflags env, impl = comp }
      ncbl@(Cabal sects) = normalizeAndPatch env info cbl
      glob = getGlobal ncbl
      sect s@(Section "executable" _ _) | TgtExe `elem` targets env && isBuildable s = installExe env glob s
      sect s@(Section "library"    _ _) | TgtLib `elem` targets env && isBuildable s = installLib env glob s
      sect _ = return ()
  message env 3 $ "Unnormalized Cabal file:\n" ++ show cbl
  message env 2 $ "Normalized Cabal file:\n" ++ show ncbl
  mapM_ sect $ addMissing sects

installExe :: Env -> Section -> Section -> IO ()
installExe env glob sect@(Section _ name _) = do
  message env 0 $ "Installing executable " ++ name
  installDataFiles env glob sect
  installIncludeFiles env glob sect
  installCFiles env glob sect
  installPkgExe (backend env) env glob sect

installLib :: Env -> Section -> Section -> IO ()
installLib env glob sect@(Section _ name _) = do
  message env 0 $ "Installing library " ++ name
  installDataFiles env glob sect
  installIncludeFiles env glob sect
  installCFiles env glob sect
  installPkgLib (backend env) env glob sect

installDataFiles :: Env -> Section -> Section -> IO ()
installDataFiles env glob@(Section _ _ gflds) sect@(Section _ _ flds) = do
  let gdatas = getFieldStrings gflds [] "data-files"
      datas  = getFieldStrings  flds [] "data-files"
      dataPrefix = makeDataPrefix env glob sect
      dataDir  = dataPrefix </> "data"
  --print ("installDataFiles", gdatas ++ datas, dataDir)
  case gdatas ++ datas of
    [] -> return ()
    pats -> do
      files <- matchFiles "." pats
      message env 1 $ "Installing data files " ++ unwords files
      mkdir env dataDir
      copyFiles env "." files dataDir

installIncludeFiles :: Env -> Section -> Section -> IO ()
installIncludeFiles env glob@(Section _ _ gflds) sect@(Section _ _ flds) = do
  let gincs = getFieldStrings gflds [] "install-includes"
      incs  = getFieldStrings  flds [] "install-includes"
      dataPrefix = makeDataPrefix env glob sect
      incDir  = dataPrefix </> "include"
  case gincs ++ incs of
    [] -> return ()
    pats -> do
      let inc = head $ getFieldStrings flds ["."] "include-dirs"
      files <- matchFiles inc pats
      -- print (pats, files)
      message env 1 $ "Installing include files " ++ unwords files
      mkdir env incDir
      copyFiles env inc files incDir

installCFiles :: Env -> Section -> Section -> IO ()
installCFiles env glob@(Section _ _ gflds) sect@(Section _ _ flds) = do
  let gcs = getFieldStrings gflds [] "c-sources"
      cs  = getFieldStrings  flds [] "c-sources"
      dataPrefix = makeDataPrefix env glob sect
      cDir  = dataPrefix </> "cbits"
  case gcs ++ cs of
    [] -> return ()
    files -> do
      message env 1 $ "Installing C files " ++ unwords files
      mkdir env cDir
      mapM_ (\ f -> cp env f cDir) files

-----------------------------------------

cmdHelp :: Env -> [String] -> IO ()
cmdHelp _ _ = putStrLn "\
  \Available commands:\n\
  \  mcabal [FLAGS] build [--git=URL] [PKG]    build in current directory, or the package PKG\n\
  \  mcabal [FLAGS] clean                      clean in the current directory\n\
  \  mcabal [FLAGS] fetch [--git=URL] PKG      fetch files for package PKG\n\
  \  mcabal [FLAGS] help                       show this message\n\
  \  mcabal [FLAGS] install [--git=URL] [PKG]  build and install in current directory, or the package PKG\n\
  \  mcabal [FLAGS] parse FILE                 just parse a Cabal file (for debugging)\n\
  \  mcabal [FLAGS] update                     retrieve new set of consistent packages from Stackage\n\
  \\n\
  \Flags:\n\
  \  --version                     show version\n\
  \  -fFLAGS                       set cabal flags\n\
  \  -v                            be more verbose (can be repeated)\n\
  \  -q                            be quiet\n\
  \  -r                            do recursive installs for missing packages\n\
  \  --ghc                         compile using ghc\n\
  \  --mhs                         compile using mhs (default)\n\
  \  --git=URL                     fetch from the Git repo instead of hackage\n\
  \  --dry-run                     do NOT execute the commands, just print\n\
  \\n\
  \Installs go to $CABALDIR if set, otherwise $HOME/.mcabal.\n\
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
      info = FlagInfo { os = I.os, arch = I.arch, flags = eflags env, impl = comp }
      ncbl = normalizeAndPatch env info cbl
  putStrLn "Unnormalized:"
  putStrLn $ showCabal cbl
  putStrLn "Normalized:"
  putStrLn $ showCabal ncbl
cmdParse _ _ = error "cmdParse"

normalizeAndPatch :: Env -> FlagInfo -> Cabal -> Cabal
normalizeAndPatch env flags = patchDepends (backend env) . normalize flags
