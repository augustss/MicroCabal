module MicroCabal.Env(
  Env(..),
  Backend(..),
  PackageName,
  ) where
import MicroCabal.Cabal
import MicroCabal.StackageList(PackageName)

data Env = Env {
  cabalDir :: FilePath,
  distDir  :: FilePath,
  verbose  :: Int,
  backend  :: Backend
  }

data Backend = Backend {
  backendName    :: String,                                 -- name of backaned
  doesPkgExist   :: Env -> PackageName        -> IO Bool,   -- is the package available in the database?
  buildPkgExe    :: Env -> Section -> Section -> IO (),     -- build executable the current directory
  buildPkgLib    :: Env -> Section -> Section -> IO (),     -- build the package in the current directory
  installPkgExe  :: Env -> Section -> Section -> IO (),     -- install the package from the current directory
  installPkgLib  :: Env -> Section -> Section -> IO ()      -- install the package from the current directory
  }
