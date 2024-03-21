module MicroCabal.Main where
import System.Environment
import qualified System.Info as I
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
  return Env{ cabalDir = cdir, verbose = 0 }

decodeCommonArgs :: Env -> IO (Env, [String])
decodeCommonArgs env = do
  let loop e ("-v" : as) = loop e{ verbose = verbose e + 1 } as
      loop e as = return (e, as)
  loop env =<< getArgs

usage :: IO ()
usage = putStrLn "\
\Usage:\n\
\  mcabal [-v] upgrade\n\
\  mcabal [-v] build\n\
\  mcabal [-v] install\n\
\"

-----------------------------------------

-- XX This needs improvement
getBestStackage :: Env -> IO URL
getBestStackage _ = return $ URL "https://raw.githubusercontent.com/commercialhaskell/stackage-snapshots/master/lts/22/13.yaml"

stackageName :: FilePath
stackageName = "stackage.yaml"

cmdUpgrade :: Env -> [String] -> IO ()
cmdUpgrade env _args = do
  let dir = cabalDir env
      stk = dir ++ "/" ++ stackageName
  mkdir env dir
  url <- getBestStackage env
  wget env url stk
  file <- readFile stk
  let yml = parseYAML stk file
  putStrLn $ showYAML yml
  putStrLn $ show $ yamlToStackageList yml

cmdTest :: Env -> [String] -> IO ()
cmdTest _ _ = do
  let stk = "11.yaml"
  file <- readFile stk
  let yml = parseYAML stk file
  putStrLn $ showYAML yml


-----------------------------------------

cmdBuild :: Env -> [String] -> IO ()
cmdBuild _env _args = undefined

cmdInstall :: Env -> [String] -> IO ()
cmdInstall _env _args = undefined
