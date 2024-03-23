module MicroCabal.Unix(
  cmd, tryCmd, cmdOut,
  mkdir,
  wget, URL(..),
  tarx,
  rmrf,
  ) where
import Control.Exception
import Control.Monad
import Data.Maybe
import System.Directory
import System.Environment
import System.IO
import System.Process(callCommand)
import MicroCabal.Env

newtype URL = URL String

cmd :: Env -> String -> IO ()
cmd env s = do
  when (verbose env > 1) $
    putStrLn $ "cmd: " ++ s
  callCommand s

tryCmd :: Env -> String -> IO Bool
tryCmd env s = catch (cmd env s >> return True) (\ (_ :: SomeException) -> return False)

cmdOut :: Env -> String -> IO String
cmdOut env s = do
  (fn, h) <- tmpFile
  hClose h
  cmd env $ s ++ ">" ++ fn
  o <- readFile fn
  removeFile fn
  return o

tmpFile :: IO (String, Handle)
tmpFile = do
  mtmp <- lookupEnv "TMPDIR"
  let tmp = fromMaybe "/tmp" mtmp
      tmplt = "mcabal.txt"
  res <- try $ openTempFile tmp tmplt
  case res of
    Right x -> return x
    Left (_::SomeException) -> openTempFile "." tmplt


---------

-- Create a directory path, don't complain if it exists.
mkdir :: Env -> String -> IO ()
mkdir env d = cmd env $ "mkdir -p " ++ d

-- Get a document, store it in a file.
wget :: Env -> URL -> FilePath -> IO ()
wget env (URL url) fn = cmd env $ "wget --quiet --output-document=" ++ fn ++ " " ++ url

-- Extract a tar file
tarx :: Env -> FilePath -> FilePath -> IO ()
tarx env dir file = cmd env $ "tar -C " ++ dir ++ " -x -f " ++ file

-- Recursively remove
rmrf :: Env -> FilePath -> IO ()
rmrf env fn = cmd env $ "rm -rf " ++ fn
