module MicroCabal.Unix(
  cmd, tryCmd,
  mkdir,
  wget, URL(..),
  tarx,
  rmrf,
  ) where
import Control.Exception
import Control.Monad
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
