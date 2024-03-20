module MicroCabal.StackageList(
  StackageList,
  StackagePackage(..),
  yamlToStackageList,
  ) where
import Data.Maybe
import Data.Version
import Text.Read
import MicroCabal.YAML

type StackageList = [StackagePackage]
type PackageName = String
type FlagName = String

data StackagePackage = StackagePackage {
  stName    :: PackageName,
  stVersion :: Version,
  stHidden  :: Bool,
  stFlags   :: [(FlagName, Bool)]
  }
  deriving (Show)

yamlToStackageList :: YAMLValue -> [StackagePackage]
yamlToStackageList (YRecord
  [ ("flags", YRecord flags)
  , ("hidden", YRecord hidden)
  , ("packages", YArray packages)
  , ("publish-time", _)
  , ("resolver", _)
  ]) = map (addFlags flags . addHidden hidden . decodePackage) packages
yamlToStackageList _ = error "Unrecognized Stackage package list format"

addFlags :: [(YAMLFieldName, YAMLValue)] -> StackagePackage -> StackagePackage
addFlags _ x = x

addHidden :: [(YAMLFieldName, YAMLValue)] -> StackagePackage -> StackagePackage
addHidden _ x = x

decodePackage :: YAMLValue -> StackagePackage
decodePackage (YRecord (("hackage", YString s):_)) = StackagePackage { stName = n, stVersion = v, stHidden = False, stFlags = [] }
  where
    (n, v) =
      case span (/= '-') (reverse s) of
        (rv, rn) -> (reverse (drop 1 rn), fromMaybe (error "bad version") $ readVersion (reverse rv))
decodePackage y = error $ "Bad package desc " ++ show y

readVersion :: String -> Maybe Version
readVersion s = makeVersion <$> (mapM readMaybe . words . map (\ c -> if c == '.' then ' ' else c) $ s)
