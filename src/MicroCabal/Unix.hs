module MicroCabal.Unix(
  cmd, tryCmd, cmdOut, tryCmdOut,
  mkdir,
  wget, URL(..),
  tarx,
  rmrf,
  cp,
  copyFiles,
  (</>),
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
tryCmd env s = catch (cmd env s >> return True) f
  where f :: SomeException -> IO Bool
        f _ = return False

cmdOut :: Env -> String -> IO String
cmdOut env s = do
  (fn, h) <- tmpFile
  hClose h
  cmd env $ s ++ " >" ++ fn
  o <- readFile fn
  removeFile fn
  return o

tryCmdOut :: Env -> String -> IO (Maybe String)
tryCmdOut env s = do
  (fn, h) <- tmpFile
  hClose h
  b <- tryCmd env $ s ++ " >" ++ fn
  if b then do
    o <- readFile fn
    removeFile fn
    return (Just o)
   else
    return Nothing

tmpFile :: IO (String, Handle)
tmpFile = do
  mtmp <- lookupEnv "TMPDIR"
  let tmp = fromMaybe "/tmp" mtmp
      tmplt = "mcabal.txt"
  res <- try $ openTempFile tmp tmplt
  case res :: Either SomeException (String, Handle) of
    Right x -> return x
    Left  _ -> openTempFile "." tmplt


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

-- Copy a file to a directory
cp :: Env -> FilePath -> FilePath -> IO ()
cp env s d = do
  cmd env $ "rm -f " ++ d
  cmd env $ "cp " ++ s ++ " " ++ d

copyFiles :: Env -> FilePath -> [FilePath] -> FilePath -> IO ()
copyFiles env src fns tgt = do
  cmd env $ "cd " ++ src ++ "; tar cf - " ++ unwords fns ++ " | (cd " ++ tgt ++ "; tar xf - )"

-----

(</>) :: FilePath -> FilePath -> FilePath
x </> y = x ++ "/" ++ y
