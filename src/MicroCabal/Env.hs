module MicroCabal.Env(Env(..), Backend(..), PackageName) where
import MicroCabal.Cabal
import MicroCabal.StackageList(PackageName)

data Env = Env {
  cabalDir :: FilePath,
  verbose  :: Int,
  backend  :: Backend
  }

data Backend = Backend {
  doesPkgExist   :: Env -> PackageName -> IO Bool,   -- is the package available in the database?
  buildPkgExe    :: Env -> String -> [Field] -> IO (),     -- build the package in the current directory
  installPkgExe  :: Env -> Cabal       -> IO (),     -- install the package from the current directory
  buildPkgLib    :: Env -> Cabal       -> IO (),     -- build the package in the current directory
  installPkgLib  :: Env -> Cabal       -> IO ()      -- install the package from the current directory
  }
