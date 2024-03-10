module MicroCabal.Main where
import Data.List
import System.Environment
import qualified System.Info as I
import MicroCabal.Cabal
import MicroCabal.Normalize
import MicroCabal.Parse
import Text.ParserComb

main :: IO ()
main = do
  args <- getArgs
  let fn = args !! 0
  rfile <- readFile fn
  let file = dropComments rfile
      cbl :: Cabal
      cbl =
        case runPrsr pTop (initLexState file) of
          Left (LastFail n ts msgs) -> error $ "\n" ++
            "  found:    " ++ (map show ts ++ ["EOF"]) !! 0 ++ "\n" ++
            "  expected: " ++ unwords (nub msgs) ++ "\n" ++
--            "  n=" ++ show n ++ "\n" ++
            "  line: " ++ show (1 + length (filter (== '\n') (take (length file - n) file))) ++ "\n" ++
            "  at: " ++ show (drop (length file - n) file)
          Right (a:_) -> a
          Right []    -> undefined -- impossible
  let info = FlagInfo { os = I.os, arch = I.arch, flags = [], impl = (I.compilerName, I.compilerVersion) }
      ncbl = normalize info cbl
  putStrLn $ showCabal cbl
  putStrLn $ showCabal ncbl
