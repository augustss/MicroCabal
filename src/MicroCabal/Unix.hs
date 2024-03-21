module MicroCabal.Unix(
  mkdir,
  wget, URL(..),
  ) where
import Control.Monad
import System.Process
import MicroCabal.Env

newtype URL = URL String

cmd :: Env -> String -> IO ()
cmd env s = do
  when (verbose env > 1) $
    putStrLn $ "cmd: " ++ s
  callCommand s

-- Create a directory path, don't complain if it exists.
mkdir :: Env -> String -> IO ()
mkdir env d = cmd env $ "mkdir -p " ++ d

-- Get a document, store it in a file.
wget :: Env -> URL -> FilePath -> IO ()
wget env (URL url) fn = cmd env $ "wget --quiet --output-document=" ++ fn ++ " " ++ url
