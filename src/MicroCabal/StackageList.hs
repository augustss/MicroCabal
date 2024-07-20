module MicroCabal.StackageList(
  StackageList,
  StackagePackage(..),
  PackageName, FlagName,
  showPackage,
  readPackage,
  yamlToStackageList,
  yamlToGHCVersion,
  readVersionM,
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

showPackage :: StackagePackage -> String
showPackage st = unwords $ stName st : showVersion (stVersion st) : show (stHidden st) : map flag (stFlags st)
  where flag (n,b) = n ++ "=" ++ show b

readPackage :: String -> StackagePackage
readPackage spkg =
  case words spkg of
    name : vers : hide : flgs ->
      StackagePackage { stName = name, stVersion = readVersion vers, stHidden = read hide, stFlags = map flag flgs }
    x -> error $ "readPackage" ++ show x
  where flag s = (n, read (drop 1 b)) where (n, b) = span (/= '=') s

yamlToStackageList :: YAMLValue -> [StackagePackage]
yamlToStackageList (YRecord flds) =
  let lookf s = fromMaybe (error $ "yamlToStackageList: no " ++ s) $ lookup s flds
  in  case (lookf "flags", lookf "hidden", lookf "packages") of
        (YRecord flags, YRecord hidden, YArray packages) -> 
          map (addFlags flags . addHidden hidden . decodePackage) packages
        _ -> error "Unrecognized Stackage package list format"
yamlToStackageList _ = error "Unrecognized Stackage package list format"

-- XXX Ugly, ugly hack because the YAML parser is brtoken.
yamlToGHCVersion :: YAMLValue -> String
yamlToGHCVersion (YRecord flds) =
  let bad n = error "yamlToGHCVersion: Unrecognized Stackage package list format " ++ show (n::Int)
  in  case lookup "packages" flds of
        Just (YArray packages) ->
          case last packages of
            YRecord pflds ->
              case lookup "resolver" pflds of
                Just (YRecord rflds) ->
                  case lookup "compiler" rflds of
                    Just (YString s) -> s
                    _ -> bad 1
                _ -> bad 2
            _ -> bad 3
        _ -> bad 4
yamlToGHCVersion _ = error "Unrecognized Stackage package list format"

addFlags :: [(YAMLFieldName, YAMLValue)] -> StackagePackage -> StackagePackage
addFlags flags st = maybe st (\ f -> st{ stFlags = yflags f }) $ lookup (stName st) flags
  where yflags (YRecord r) = [(n, b) | (n, YBool b) <- r]
        yflags _ = error "addFlags"

addHidden :: [(YAMLFieldName, YAMLValue)] -> StackagePackage -> StackagePackage
addHidden hidden st = maybe st (\ f -> st{ stHidden = ybool f }) $ lookup (stName st) hidden
  where ybool (YBool b) = b
        ybool _ = error "addHidden"

decodePackage :: YAMLValue -> StackagePackage
decodePackage (YRecord (("hackage", YString fs):_)) = StackagePackage { stName = n, stVersion = v, stHidden = False, stFlags = [] }
  where
    s = takeWhile (/= '@') fs
    (n, v) =
      case span (/= '-') (reverse s) of
        (rv, rn) -> (reverse (drop 1 rn), readVersion (reverse rv))
decodePackage y = error $ "Bad package desc " ++ show y

readVersion :: String -> Version
readVersion s = fromMaybe (error $ "readVersion: bad version " ++ s) $ readVersionM s

readVersionM :: String -> Maybe Version
readVersionM s = makeVersion <$> (mapM readMaybe . words . map (\ c -> if c == '.' then ' ' else c) $ s)
