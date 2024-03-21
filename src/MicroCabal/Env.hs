module MicroCabal.Env(Env(..)) where

data Env = Env {
  cabalDir :: FilePath,
  verbose  :: Int
  }
