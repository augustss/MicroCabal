module MicroCabal.Cabal where

--type ExecName = String
type FieldName = String
type Name = String

data Cabal = Cabal {
  globalFields :: [Field],
  sections     :: [Section]
  }
  deriving (Show)

data Value
  = VItems   [Item]
  | VItem    Item
  | VBool    Bool
  | VVersion Version
  deriving (Show)

data Field
  = Field FieldName Value
  | If Cond [Field] [Field]
  deriving (Show)

type Cond = String

data Section
  = Common     Name [Field]
  | Library    (Maybe Name) [Field]
  | Executable Name [Field]
  | SourceRepo Name [Field]
  deriving (Show)

newtype Version = Version [String]
  deriving (Show)

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

showCabal :: Cabal -> String
showCabal cbl =
  "Cabal\n" ++
  "globalFields=\n" ++
  unlines (map showField (globalFields cbl)) ++
  unlines (map showSection (sections cbl))

showField :: Field -> String
showField (Field n v) = "  Field " ++ n ++ ": " ++ show v
showField (If c t e) =
  "  If " ++ c ++ "\n" ++
  unlines (map (indent . showField) t) ++
  if null e then "" else
    "  Else\n" ++
    unlines (map (indent . showField) e)

indent :: String -> String
indent s = "  " ++ concatMap (\ c -> if c == '\n' then "\n  " else [c]) s

showSection :: Section -> String
showSection (Common n fs) = unlines $ ("  Common " ++ n) : map (indent . showField) fs 
showSection (Library n fs) = unlines $ ("  Library " ++ show n) : map (indent . showField) fs
showSection (Executable n fs) = unlines $ ("  Executable " ++ n) : map (indent . showField) fs
showSection (SourceRepo n fs) = unlines $ ("  SourceRepo " ++ n) : map (indent . showField) fs
