module MicroCabal.Backend.MHS(mhsBackend) where
import Control.Monad
import Data.List(dropWhileEnd, (\\), stripPrefix)
import Data.Maybe
import Data.Version
import System.Directory
import MicroCabal.Cabal
import MicroCabal.Env
import MicroCabal.Macros
import MicroCabal.Parse(readVersion)
import MicroCabal.Unix
import System.Environment

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
    buildPkgExe = mhsBuildExe,
    buildPkgLib = mhsBuildLib,
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
  ("array",     makeVersion [0,5,6,0]),
  ("base",      makeVersion [4,19,1,0]),
  ("deepseq",   makeVersion [1,5,0,0]),
  ("directory", makeVersion [1,3,8,1]),
  ("hashable",  makeVersion [1,0,0,0]),    -- very rudimentary
  ("process",   makeVersion [1,6,18,0]),
  ("bytestring",makeVersion [0,12,1,0]),
  ("text",      makeVersion [2,1,1])
  ]

getPackageVersion :: Env -> String -> IO Version
getPackageVersion _ pkgName | Just v <- lookup pkgName builtinPackages = return v
getPackageVersion env pkgName = do
  dir <- getMhsDir env
  pkgs <- listDirectory (dir </> "packages")
  let n = pkgName ++ "-"
  case [ readVersion vers | p <- pkgs, Just verspkg <- [stripPrefix n p], Just vers <- [stripSuffix ".pkg" verspkg] ] of
    [v] -> return v
    []  -> error $ "Not installed: " ++ pkgName
    _   -> error $ "Multiple version: " ++ pkgName

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
  let macros = genPkgVersionMacros (zip deps depvers)
  return $ -- ["-i"] ++
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
mhsBuildExe env _ (Section _ name flds) = do
  initDB env
  let mainIs  = getFieldString  flds         "main-is"
      srcDirs = getFieldStrings flds ["."]   "hs-source-dirs"
      bin     = distDir env </> binMhs </> name
  mkdir env $ distDir env </> binMhs
  mainIs' <- findMainIs env srcDirs mainIs
  stdArgs <- setupStdArgs env flds
  let args    = unwords $ stdArgs ++
                          ["-a."
                          ,"-o" ++ bin, mainIs']
  when (verbose env >= 0) $
    putStrLn $ "Build " ++ bin ++ " with mhs"
  --putStrLn $ "mhs " ++ args
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

mhsBuildLib :: Env -> Section -> Section -> IO ()
mhsBuildLib env (Section _ _ glob) (Section _ name flds) = do
  initDB env
  stdArgs <- setupStdArgs env flds
  let mdls = getFieldStrings flds (error "no exposed-modules") "exposed-modules"
      omdls = getFieldStrings flds [] "other-modules"
      vers = getVersion glob "version"
      namever = name ++ "-" ++ showVersion vers
      pkgfn = namever ++ ".pkg"
      args = unwords $ ["-P" ++ namever,
                        "-o" ++ pkgfn] ++
                       stdArgs ++
                       ["-a."] ++
                       mdls
      isMdl (' ':_) = True   -- Relies on -L output format
      isMdl _ = False
  mhs env args
  pkgmdls <- words . unlines . filter isMdl . lines <$> mhsOut env ("-L" ++ pkgfn)
  let bad = pkgmdls \\ (mdls ++ omdls)
  when (not (null bad)) $ do
    putStrLn "Warning: package modules not mentioned in exposed-modules nor other-modules"
    mapM_ putStrLn bad

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
      namever = name ++ "-" ++ showVersion vers
  mhs env $ "-Q " ++ namever ++ ".pkg"

---
-- XXX
stripSuffix :: String -> String -> Maybe String
stripSuffix suf str = reverse <$> stripPrefix (reverse suf) (reverse str)
