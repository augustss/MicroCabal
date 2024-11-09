{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module MicroCabal.Macros(genPkgVersionMacros) where
import Data.Version

genPkgVersionMacros :: [(String, Version)] -> [String]
genPkgVersionMacros pkgs =
  concatMap (\ (name, vers) -> generateMacros (map fixchar name) vers) pkgs
  where
    fixchar '-' = '_'
    fixchar c = c

generateMacros :: String -> Version -> [String]
generateMacros name version =
  [ concat [ "'-DVERSION_", name, "=", show (showVersion version), "'" ]
  , concat [ "'-DMIN_VERSION_", name, "(x,y,z)=("
           , "(x)<", major1, "||"
           , "(x)==", major1, "&&(y)<", major2, "||"
           , "(x)==", major1, "&&(y)==", major2, "&&(z)<=", minor
           , ")'"
           ]
  ]
  where
    (major1:major2:minor:_) = map show (versionBranch version ++ repeat 0)
