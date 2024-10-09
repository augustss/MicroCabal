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
  [ concat [ "-DVERSION_", name, "=", show (showVersion version) ]
  , concat [ "-DMIN_VERSION_", name, "(major1,major2,minor)=("
           , "(major1)<", major1, "||"
           , "(major1)==", major1, "&&(major2)<", major2, "||"
           , "(major1)==", major1, "&&(major2)==", major2, "&&(minor)<=", minor
           , ")"
           ]
  ]
  where
    (major1:major2:minor:_) = map show (versionBranch version ++ repeat 0)
