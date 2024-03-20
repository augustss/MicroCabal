module MicroCabal.Main where
import System.Environment
import qualified System.Info as I
import MicroCabal.Cabal
import MicroCabal.Normalize
import MicroCabal.Parse
import MicroCabal.StackageList
import MicroCabal.YAML

main :: IO ()
main = do
  args <- getArgs
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
