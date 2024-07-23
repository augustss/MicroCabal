module MicroCabal.Env(
  Env(..),
  Backend(..),
  PackageName,
  message,
  ) where
import MicroCabal.Cabal
import MicroCabal.StackageList(PackageName)

data Env = Env {
  cabalDir :: FilePath,           -- where to install, default is $HOME/.mcabal
  distDir  :: FilePath,           -- where to build, default is dist-mcabal
  verbose  :: Int,                -- how chatty, default is 0, -1=say nothing, 0=minimal messages, 1=debug info
  depth    :: Int,                -- nesting depth for recursive builds, default is 0
  recursive:: Bool,               -- do recursive builds, default is False
  backend  :: Backend             -- which compiler to use, default is MHS
  }

data Backend = Backend {
  backendNameVers:: Env ->                       IO (String, Version), -- name and version
  doesPkgExist   :: Env -> PackageName        -> IO Bool,   -- is the package available in the database?
  buildPkgExe    :: Env -> Section -> Section -> IO (),     -- build executable the current directory
  buildPkgLib    :: Env -> Section -> Section -> IO (),     -- build the package in the current directory
  installPkgExe  :: Env -> Section -> Section -> IO (),     -- install the package from the current directory
  installPkgLib  :: Env -> Section -> Section -> IO ()      -- install the package from the current directory
  }

message :: Env -> Int -> String -> IO ()
message env level msg | verbose env >= level = putStrLn $ replicate (2 * depth env) ' ' ++ msg
                      | otherwise = return ()
