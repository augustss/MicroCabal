module MicroCabal.Cabal(
  Version(..), makeVersion,
  FieldName, Name,
  Cabal(..),
  Value(..),
  Field(..),
  Cond(..),
  Section(..),
  SectionType,
  VersionRange(..),
  Item,
  FlagInfo(..),
  showCabal,
  getFieldString,
  getFieldStrings,
  ) where
import Data.Version

--type ExecName = String
type FieldName = String
type Name = String

newtype Cabal = Cabal [Section]
  deriving (Show)

data Value
  = VItems   [Item]
  | VItem    Item
  | VBool    Bool
  | VVersion Version
  | VRange   VersionRange
  | VPkgs    [(Item, [Item], Maybe VersionRange)]
  deriving (Show)

data Field
  = Field FieldName Value
  | If Cond [Field] [Field]
  deriving (Show)

data Cond
  = CBool Bool
  | Cos Item
  | Carch Item
  | Cimpl Item (Maybe VersionRange)
  | Cflag Item
  | Cnot Cond
  | Cand Cond Cond
  | Cor  Cond Cond
  deriving (Show)

data Section = Section SectionType Name [Field]
  deriving (Show)

type SectionType = String

data VersionRange
  = VEQ Version
  | VGT Version
  | VLT Version
  | VLE Version
  | VGE Version
  | VGEHat Version
  | VEQWild Version
  | VOr VersionRange VersionRange
  | VAnd VersionRange VersionRange
  | VEQSet [Version]
  | VGEHatSet [Version]
  deriving (Show)

type Item = String

data FlagInfo = FlagInfo
  { os    :: String
  , arch  :: String
  , impl  :: (String, Version)
  , flags :: [(Name, Bool)]
  }
  deriving (Show)

showCabal :: Cabal -> String
showCabal (Cabal sects) =
  "Cabal\n" ++  unlines (map showSection sects)

showField :: Field -> String
showField (Field n v) = "  Field " ++ n ++ ": " ++ show v
showField (If c t e) =
  "  If " ++ show c ++ "\n" ++
  unlines (map (indent . showField) t) ++
  if null e then "" else
    "  Else\n" ++
    unlines (map (indent . showField) e)

indent :: String -> String
indent s = "  " ++ concatMap (\ c -> if c == '\n' then "\n  " else [c]) s

showSection :: Section -> String
showSection (Section s n fs) = unlines $ ("  " ++ s ++ " " ++ n) : map (indent . showField) fs 

getFieldString :: [Field] -> FieldName -> String
getFieldString flds n =
  case [ s | Field f (VItem s) <- flds, f == n ] of
    [s] -> s
    _   -> error $ "field not found: " ++ show n

getFieldStrings :: [Field] -> FieldName -> [String]
getFieldStrings flds n =
  case [ ss | Field f (VItems ss) <- flds, f == n ] of
    [ss] -> ss
    _    -> error $ "field not found: " ++ show n
