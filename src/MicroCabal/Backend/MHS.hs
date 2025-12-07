module MicroCabal.Backend.MHS(mhsBackend) where
import Control.Monad
import Data.List(dropWhileEnd, (\\), stripPrefix)
import Data.Maybe
import Data.Version
import System.Directory
import MicroCabal.Cabal
import MicroCabal.Env
import MicroCabal.Macros
import MicroCabal.StackageList(readVersionM, readVersion)
import MicroCabal.Unix
import System.Environment
import Debug.Trace

mhsBackend :: Env -> IO Backend
mhsBackend env = do
  mmhs <- lookupEnv "MHS"
  let exe = fromMaybe "mhs" mmhs
  numVersion <- takeWhile (/= '\n') <$> cmdOut env (exe ++ " --numeric-version")
  let mhsVersion = "mhs-" ++ numVersion
      version = readVersion numVersion
  return Backend {
    compilerName = "mhs",
    compilerVersion = version,
    compiler = mhsVersion,
    compilerExe = exe,
    doesPkgExist = mhsExists,
    patchDepends = mhsPatchDepends,
    patchName = mhsPatchName,
    buildPkgExe = mhsBuildExe,
    buildPkgLib = mhsBuildLib,
    buildPkgForLib = mhsBuildForeignLib,
    installPkgExe = mhsInstallExe,
    installPkgLib = mhsInstallLib
    }

mhsNameVers :: Env -> IO (String, Version)
mhsNameVers env = do
  v <- readVersion . takeWhile (/= '\n') <$> mhsOut env "--numeric-version"
  return ("mhs", v)

getMhsDir :: Env -> IO FilePath
getMhsDir env = do
  (n, v) <- mhsNameVers env
  return $ cabalDir env ++ "/" ++ n ++ "-" ++ showVersion v

initDB :: Env -> IO ()
initDB env = do
  dir <- getMhsDir env
  b <- doesDirectoryExist dir
  when (not b) $ do
    mkdir env (dir </> "packages")

mhsExists :: Env -> PackageName -> IO Bool
mhsExists _ pkgname | Just _ <- lookup pkgname builtinPackages = return True
mhsExists env pkgname = do
  initDB env
  dir <- getMhsDir env
  pkgs <- listDirectory $ dir </> "packages"
  return $ any ((== pkgname) . init . dropWhileEnd (/= '-')) pkgs

-- XXX These packages are part of mhs.
-- The version numbers are totally fake.
-- The version numbers are from GHC 9.8.2
builtinPackages :: [(String, Version)]
builtinPackages = [
--  ("array",     makeVersion [0,5,6,0]),  has its own package
  ("base",      makeVersion [4,19,1,0]),
  ("bytestring",makeVersion [0,12,1,0]),
  ("deepseq",   makeVersion [1,5,0,0]),
  ("directory", makeVersion [1,3,8,1]),
  ("hashable",  makeVersion [1,0,0,0]),    -- very rudimentary
  ("process",   makeVersion [1,6,18,0]),
  ("text",      makeVersion [2,1,1]),
  ("stm",       makeVersion [2,5,3,0])
  ]

getPackageVersion :: Env -> String -> IO Version
getPackageVersion _ pkgName | Just v <- lookup pkgName builtinPackages = return v
getPackageVersion env pkgName = do
  dir <- getMhsDir env
  pkgs <- listDirectory (dir </> "packages")
  let n = pkgName ++ "-"
  case [ v | p <- pkgs,
             Just verspkg <- [stripPrefix n p],
             Just vers <- [stripSuffix ".pkg" verspkg],
             Just v <- [readVersionM vers]
       ] of
    []  -> error $ "Not installed: " ++ pkgName
    [v] -> return v
    vs  -> error $ "Multiple version: " ++ pkgName ++ show vs

setupStdArgs :: Env -> [Field] -> IO [String]
setupStdArgs env flds = do
  -- db <- getMhsDir env
  let srcDirs = getFieldStrings flds ["."]   "hs-source-dirs"
      defExts = getFieldStrings flds []      "default-extensions"
      exts    = getFieldStrings flds defExts "extensions"
      oexts   = getFieldStrings flds []      "other-extensions"
      opts    = getFieldStrings flds []      "mhs-options"
      cppOpts = getFieldStrings flds []      "cpp-options"
      incs    = getFieldStrings flds []      "include-dirs"
      exts'   = filter (`elem` mhsX) (exts ++ oexts)
      deps    = getBuildDependsPkg flds
      mhsX    = ["CPP"]
  depvers <- mapM (getPackageVersion env) deps
  let macros = genPkgVersionMacros $ revPatchDepends $ zip deps depvers
  return $ ["-i"] ++
    map ("-i" ++) srcDirs ++
    ["-i" ++ pathModuleDir env] ++
    map ("-X" ++) exts' ++
    map ("-I" ++) incs ++
    opts ++
    macros ++
    cppOpts

binMhs :: String
binMhs  = "bin" </> "mhs"

mhsBuildExe :: Env -> Section -> Section -> IO ()
mhsBuildExe env (Section _ _ gflds) (Section _ name flds) = do
  initDB env
  let mainIs  = getFieldString  flds         "main-is"
      srcDirs = getFieldStrings flds ["."]   "hs-source-dirs"
      bin     = distDir env </> binMhs </> name
      gcs = getFieldStrings gflds [] "c-sources"
      cs  = getFieldStrings  flds [] "c-sources"
      csrc = gcs ++ cs
  mkdir env $ distDir env </> binMhs
  mainIs' <- findMainIs env srcDirs mainIs
  stdArgs <- setupStdArgs env flds
  let args    = unwords $ stdArgs ++
                          csrc ++
                          ["-z", "-a.","-o" ++ bin, mainIs']
  message env 0 $ "Build " ++ bin ++ " with mhs"
  mhs env args

mhs :: Env -> String -> IO ()
mhs env args = do
  let flg = if verbose env == 1 then "-l " else if verbose env > 1 then "-v " else ""
  cmd env $ compilerExe (backend env) ++ " " ++ flg ++ args

mhsOut :: Env -> String -> IO String
mhsOut env args =
  cmdOut env $ compilerExe (backend env) ++ " " ++ args

findMainIs :: Env -> [FilePath] -> FilePath -> IO FilePath
findMainIs _ [] fn = error $ "cannot find " ++ show fn
findMainIs env (d:ds) fn = do
  let fn' = d </> fn
  b <- doesFileExist fn'
  if b then
    return fn'
   else
    findMainIs env ds fn

mhsBuildForeignLib :: Env -> Section -> Section -> IO ()
mhsBuildForeignLib env (Section _ _ glob) (Section _ name flds) = do
  initDB env
  stdArgs <- setupStdArgs env flds
  let omdls = getFieldStrings flds (error "no other-modules") "other-modules"
      vers = getVersion glob "version"
      namever = name ++ "-" ++ showVersion vers
      pkgfn = distDir env </> "lib" ++ namever ++ ".so"
      args = unwords $ ["-c", "-optc", "--shared", "-optc", "-fPIC", "-o" ++ pkgfn] ++ stdArgs ++ omdls
  mhs env args

mhsBuildLib :: Env -> Section -> Section -> IO ()
mhsBuildLib env (Section _ _ glob) (Section _ name flds) = do
  initDB env
  stdArgs <- setupStdArgs env flds
  let mdls = getFieldStrings flds [] "exposed-modules"
      omdls = getFieldStrings flds [] "other-modules"
      vers = getVersion glob "version"
      namever = name ++ "-" ++ showVersion vers
      pkgfn = distDir env ++ "/" ++ namever ++ ".pkg"
      cs  = getFieldStrings flds [] "c-sources"
      ldf = getFieldStrings flds [] "extra-libraries"
      args = unwords $ map ("-optc " ++) cs ++
                       map ("-optl -l" ++) ldf ++
                       ["-P" ++ namever,
                        "-o" ++ pkgfn] ++
                       stdArgs ++
                       ["-a."] ++
                       mdls
      isMdl (' ':_) = True   -- Relies on -L output format
      isMdl _ = False
  when (null mdls) $
    message env (-1) "Warning: exposed-modules is empty"  
  mhs env args
  pkgmdls <- words . unlines . filter isMdl . lines <$> mhsOut env ("-L" ++ pkgfn)
  let bad = pkgmdls \\ (mdls ++ omdls)
  when (not (null bad)) $ do
    message env (-1) "Warning: package modules not mentioned in exposed-modules nor other-modules"
    mapM_ (message env (-1)) bad

mhsInstallExe :: Env -> Section -> Section -> IO ()
mhsInstallExe env (Section _ _ _glob) (Section _ name _) = do
  let bin = distDir env </> binMhs </> name
      binDir = cabalDir env </> "bin"
  mkdir env binDir
  cpr env bin (binDir </> name)

mhsInstallLib :: Env -> Section -> Section -> IO ()
mhsInstallLib env (Section _ _ glob) (Section _ name _) = do
  initDB env
  let vers = getVersion glob "version"
      namever = distDir env ++ "/" ++ name ++ "-" ++ showVersion vers
  mhs env $ "-Q " ++ namever ++ ".pkg"

---
-- XXX
stripSuffix :: String -> String -> Maybe String
stripSuffix suf str = reverse <$> stripPrefix (reverse suf) (reverse str)

-- Update build-depends for packages that have a special mhs version, also add ghc-compat.
mhsPatchDepends :: Cabal -> Cabal
mhsPatchDepends cbl@(Cabal sects) = Cabal (map patchSect sects)
  where
    patchSect (Section styp sname flds) = Section styp sname (map patchField flds)
    patchField (Field "build-depends" (VPkgs ds)) = Field "build-depends" (VPkgs (mhsExtraPkgs cbl ++ map patchDep ds))
    patchField fld = fld
    patchDep d@(pkg, xs, _mv) | n /= pkg = (n, xs, Just (VEQ v))
                              | otherwise = d
      where (n, v) = mhsPatchName (pkg, undefined)

-- Add a dependency on ghc-compat
mhsExtraPkgs :: Cabal -> [(Item, [Item], Maybe VersionRange)]
mhsExtraPkgs cbl | forMhs (getCabalName cbl) = []
                 | otherwise = [ ("ghc-compat", [], Nothing) ]
  where forMhs n = n `elem` ["base", "ghc-compat", "MicroHs", "MicroCabal"]

mhsPatchName :: (Name, Version) -> (Name, Version)
mhsPatchName (n, _) | Just nv <- lookup n mhsPackages =
  trace ("Changing package " ++ n ++ " to " ++ n ++ "-mhs") $
  nv
mhsPatchName nv = nv

mhsPackages :: [(Name, (Name, Version))]
mhsPackages =
  [ ("array",  ("array-mhs",  makeVersion [0,5,8,0]))
  , ("random", ("random-mhs", makeVersion [1,3,2,1]))
  ]

-- Temporary hack: undo dependency patching
revPatchDepends :: [(String, Version)] -> [(String, Version)]
revPatchDepends svs = [(rev s, v) | (s, v) <- svs]
  where rev s = fromMaybe s $ lookup s revm
        revm = [(r, n) | (n, (r, _)) <- mhsPackages]
