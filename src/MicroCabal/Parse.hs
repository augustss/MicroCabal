module MicroCabal.Parse(
  parseCabal,
  parseYAML,
  parseSnapshots,
  readVersion,
  ) where
import Control.Applicative
import Control.Monad
import Data.Char
import Data.List
import Data.Maybe
import Data.Version
import Text.ParserComb
import MicroCabal.Cabal
import MicroCabal.YAML
--import Debug.Trace

parseCabal :: FilePath -> String -> Cabal
parseCabal fn rfile = runP pCabalTop fn $ dropCabalComments rfile

parseYAML :: FilePath -> String -> YAMLValue
parseYAML fn rfile = runP pYAMLTop fn $ dropYAMLComments rfile

parseSnapshots :: FilePath -> String -> [(String, String)]
parseSnapshots fn rfile = runP pSnapshotsTop fn rfile

runP :: P a -> FilePath -> String -> a
runP prsr fn file =
  case runPrsr prsr (initLexState file) of
    Left (LastFail n ts msgs) -> error $ "\n" ++
      "  found:    " ++ (map show ts ++ ["EOF"]) !! 0 ++ "\n" ++
      "  expected: " ++ unwords (nub msgs) ++ "\n" ++
--      "  n=" ++ show n ++ "\n" ++
      "  file: " ++ show fn ++ "\n" ++
      "  line: " ++ show (1 + length (filter (== '\n') (take (length file - n) file))) ++ "\n" ++
      "  at: " ++ show (take 100 (drop (length file - n) file))
    Right (a:_) -> a
    Right []    -> undefined -- impossible


------------------------------

type P a = Prsr LexState Char a

data LexState = LS Int [Int] [Char]
  deriving (Show)

initLexState :: [Char] -> LexState
initLexState cs = LS 0 [] cs

end :: Char
end = '\EOT'
fieldSep :: Char
fieldSep = '\FS'

instance TokenMachine LexState Char where
--  tmNextToken ls | trace ("tmNextToken: " ++ show ls) False = undefined
  tmNextToken ls@(LS _ [] []) = (end, ls)         -- ugly hack
  tmNextToken (LS i (_:ks) []) = (fieldSep, LS i ks [])
  tmNextToken (LS i [] (c:cs)) | c == '\n' = (c, LS     0 [] cs)
                               | otherwise = (c, LS (i+1) [] cs)
  tmNextToken (LS i kks@(k:ks) (c:cs)) | c /= '\n' = (c, LS (i+1) kks cs)
                                       | otherwise =
    case skipEmpty cs of
      Just cs' -> tmNextToken (LS i kks cs')
      _ ->
        let lead 0     _             = ('\n', LS 0 kks cs)              -- There are at least k leading spaces
            lead j (x:xs) | x == ' ' = lead (j-1) xs                    -- Count spaces
            lead _     _             = (fieldSep, LS 0 ks (c:cs))       -- Fewer than k spaces.  Generate FS, pop, and try again.
        in  lead (k+1) cs
  tmRawTokens (LS _ _ cs) = cs

skipEmpty :: String -> Maybe String
skipEmpty s =
  case dropWhile (== ' ') s of
    cs@('\n':_) -> Just cs
    _ -> Nothing

pushColumn :: P ()
pushColumn = mapTokenState (\ (LS i ks cs) -> LS i (i:ks) cs)

pushFieldSep :: P ()
pushFieldSep = mapTokenState (\ (LS i ks cs) -> LS i ks (fieldSep:cs))

lower :: String -> String
lower = map toLower

-- Change lines with first non-space being '--' into just a newline
dropCabalComments :: String -> String
dropCabalComments = unlines . map cmt . lines
  where
    cmt s | take 2 (dropWhile (== ' ') s) == "--" = ""
          | otherwise = s

satisfySome :: String -> (Char -> Bool) -> P [Char]
satisfySome msg p = (:) <$> satisfy msg p <*> satisfyMany p

----------------------------------------------------------------

pCabalTop :: P Cabal
pCabalTop = pCabal <* pWhite <* pChar end

pCabal :: P Cabal
pCabal = Cabal <$> ((:) <$> (Section "global" "" <$> emany pField) <*> emany pSection)

pColon :: P ()
pColon = pWhite <* pChar ':'

pWhite :: P ()
pWhite = () <$ satisfyMany (\ c -> c == ' '  || c == '\n')

pChar :: Char -> P ()
pChar c = () <$ satisfy (show c) (c ==)

pFieldSep :: P ()
pFieldSep = pChar fieldSep

pNewLine :: P ()
pNewLine = pChar '\n'

pDot :: P ()
pDot = pChar '.'

pSpaces :: P ()
pSpaces = () <$ satisfyMany (== ' ')

pIdent :: P String
pIdent = do
  c <- satisfy "ident" isAlpha_
  cs <- satisfyMany isIdent
  pure (c:cs)

pKeyWordNC :: String -> P String
pKeyWordNC s = do
  pSpaces
  i <- pIdent
  guard (s == lower i)
  pure s

isIdent :: Char -> Bool
isIdent c = isAlphaNum c || c == '-' || c == '_' || c == '\'' || c == '.'

isAlpha_ :: Char -> Bool
isAlpha_ c = isAlpha c || c == '_'

pNumber :: P Int
pNumber = read <$> satisfySome "0..9" isDigit

pParens :: P a -> P a
pParens p = pStr "(" *> p <* pStr ")"

pVersion :: P Version
pVersion = pSpaces *> (makeVersion <$> esepBy1 pNumber pDot)

pVersionRange :: P VersionRange
pVersionRange = pVOr
  where
    pVOr  = foldr1 VOr  <$> esepBy1 pVAnd (pStr "&&")
    pVAnd = foldr1 VAnd <$> esepBy1 pVOp  (pStr "||")
    pVOp  = (pVOper <*> (pSpaces *> pVersion))
        <|< pParens pVersionRange
        <|< (pStr "=="  *> pVEq)
        <|< (pStr "^>=" *> pVGEHat)
        <|< (VGE (makeVersion [0]) <$ pStr "-any")
        <|< (VLT (makeVersion [0]) <$ pStr "-none")
    pVEq  = (VEQSet <$> pVSet) <|< do
      v <- pVersion
      (VEQWild v <$ pStr ".*") <|< pure (VEQ v)
    pVSet = (pStr "{" *> pCommaList pVersion <* pStr "}")
    pVGEHat = (VGEHat <$> pVersion) <|< (VGEHatSet <$> pVSet)

pVOper :: P (Version -> VersionRange)
pVOper = pSpaces *> choice [ VGT <$ pStr ">", VLT <$ pStr "<", VGE <$ pStr ">=", VLE <$ pStr "<="]

pStr :: String -> P ()
pStr s = pSpaces *> p s
  where p "" = pure ()
        p (c:cs) = pChar c *> p cs

pStrW :: String -> P ()
pStrW s = pWhite *> pStr s

pItem :: P Item
pItem = pWhite *> (pString <|< pWord)

-- A string in quotation marks.
pString :: P String
pString = do
  pChar '"'
  let achar = satisfy "char" (\ c -> c /= '\n' && c /= end && c /= fieldSep)
      loop r = do
        c <- achar
        if c == '"' then
          return $ reverse r
         else if c /= '\\' then
          loop (c:r)
         else do
          e <- achar
          let e' = fromMaybe e $ lookup e [('n', '\n'), ('t', '\t')] -- could have more
          loop (e' : r)
  loop []

pWord :: P Item
pWord = satisfySome "word" (\ c -> c `notElem` [' ', '\n', ',', end, fieldSep])

pComma :: P ()
pComma = pWhite *> pChar ','

pCommaList :: P a -> P [a]
pCommaList p = (pStr "," *> esepBy1 p pComma)
  <|< pCommaList' p

pCommaList' :: P a -> P [a]
pCommaList' p = esepBy p pComma <* eoptional (pStr ",")

pSpaceList :: P a -> P [a]
pSpaceList p = esepBy p pWhite

pOptCommaList :: P a -> P [a]
pOptCommaList p =
    (pStrW "," *> pCommaList' p)   -- it starts with a ',', so it must be comma separated
  <|< do
    a <- p  -- parse one item
    -- now check if we have a comma or not, and pick the parser for the rest
    as <- (pStrW "," *> pCommaList' p) <|< pSpaceList p
    return (a:as)
  <|< ([] <$ pWhite)

pVComma :: P Value
pVComma = VItems <$> pCommaList pItem

pVSpace :: P Value
pVSpace = VItems <$> pSpaceList pItem

pVOptComma :: P Value
pVOptComma = VItems <$> pOptCommaList pItem

pVLibs :: P Value
pVLibs = VPkgs <$> pCommaList pPkg

pPkg :: P (Item, [Item], Maybe VersionRange)
pPkg = (,,) <$> pNameW <*> (pSpaces *> pLibs) <*> optional pVersionRange
  where
    pLibs = do
      pColon
      ((:[]) <$> pNameW) <|< (pStr "{" *> pCommaList pName <* pStr "}")
     <|<
      pure []
    pNameW = pWhite *> pIdent

pField :: P Field
pField = do
  pWhite
  pushColumn
  fn <- lower <$> pFieldName
--  traceM ("pFieldName fn=" ++ show fn)
  if fn == "if" then do
    c <- pCond
    pNewLine
    t <- emany pField
    pFieldSep
    e <- do
      pWhite
      pushColumn
      _ <- pKeyWordNC "else"
      fs <- emany pField
      pFieldSep
      pure fs
     <|<
      pure []
    pure $ If c t e
   else do
    pColon
--    traceM $ "parser " ++ fn
    let p = getParser fn
    v <- p
    pFieldSep
--    traceM ("pField v=" ++ show v)
    pure $ Field fn v

pCond :: P Cond
pCond = pCor
  where
    pCor  = foldr1 Cor  <$> esepBy1 pCand (pStr "&&")
    pCand = foldr1 Cand <$> esepBy1 pCnot (pStr "||")
    pCnot = (Cnot <$> (pStr "!" *> pCnot)) <|> pCOp
    pCOp  = (CBool <$> pBool)
        <|< (pKeyWordNC "arch" *> pParens (Carch <$> pName))
        <|< (pKeyWordNC "flag" *> pParens (Cflag <$> pName))
        <|< (pKeyWordNC "impl" *> pParens (Cimpl <$> pName <*> optional pVersionRange))
        <|< (pKeyWordNC "os"   *> pParens (Cos   <$> pName))
        <|< pParens pCond

pFreeText :: P Value
pFreeText = do
  txt <- satisfyMany (\ c -> c /= end && c /= fieldSep)
  let dot "." = ""  -- Single '.' used to make an empty line
      dot s   = s
  pure $ VItem $ unlines . map (dot . dropWhile (== ' ')) . lines $ txt

pFieldName :: P FieldName
pFieldName = pIdent

pName :: P Name
pName = pSpaces *> pIdent

pFields :: P [Field]
pFields = pSpaces *> pNewLine *> emany pField

pBool :: P Bool
pBool = (False <$ pKeyWordNC "false") <|< (True <$ pKeyWordNC "true")

pSection :: P Section
pSection = pWhite *> (
      Section <$> pKeyWordNC "common"            <*>    pName <*> pFields
  <|< Section <$> pKeyWordNC "library"           <*>  libName <*> pFields
  <|< Section <$> pKeyWordNC "executable"        <*>    pName <*> pFields
  <|< Section <$> pKeyWordNC "source-repository" <*>    pName <*> pFields
  <|< Section <$> pKeyWordNC "flag"              <*>    pName <*> pFields
  <|< Section <$> pKeyWordNC "test-suite"        <*>    pName <*> pFields
  <|< Section <$> pKeyWordNC "benchmark"         <*>    pName <*> pFields
  )
  where libName = pName <|< pure ""

getParser :: FieldName -> P Value
getParser f =
  if "x-" `isPrefixOf` f then pFreeText else
  fromMaybe (error $ "Unknown field: " ++ f) $ lookup f parsers

parsers :: [(FieldName, P Value)]
parsers =
  [ "asm-options"                    # pVSpace
  , "asm-sources"                    # pVComma
  , "autogen-includes"               # pVOptComma
  , "autogen-modules"                # pVComma
  , "build-depends"                  # pVLibs
  , "build-tool-depends"             # pVLibs  -- ??? pVComma -- XXX
  , "build-tools"                    # pVComma -- XXX
  , "buildable"                      # (VBool <$> pBool)
  , "c-sources"                      # pVComma
  , "cc-options"                     # pVComma
  , "cmm-sources"                    # pVComma
  , "cmm-options"                    # pVComma
  , "cpp-options"                    # pVOptComma
  , "cxx-options"                    # pVComma
  , "default-extensions"             # pVOptComma
  , "default-language"               # (VItem <$> pItem)
  , "exposed-modules"                # pVOptComma
  , "reexported-modules"             # pVOptComma
  , "extensions"                     # pVOptComma
  , "extra-bundled-libraries"        # pVComma
  , "extra-dynamic-library-flavours" # pVComma
  , "extra-framework-dirs"           # pVComma
  , "extra-ghci-libraries"           # pVComma
  , "extra-lib-dirs"                 # pVComma
  , "extra-lib-dirs-static"          # pVComma
  , "extra-libraries"                # pVComma
  , "frameworks"                     # pVOptComma
  , "ghc-options"                    # pVSpace
  , "ghc-prof-options"               # pVSpace
  , "ghc-shared-options"             # pVSpace
  , "ghcjs-options"                  # pVSpace
  , "ghcjs-prof-options"             # pVSpace
  , "ghcjs-shared-options"           # pVSpace
  , "hs-source-dirs"                 # pVOptComma
  , "import"                         # (VItem <$> pItem)
  , "include-dirs"                   # pVOptComma
  , "includes"                       # pVOptComma
  , "install-includes"               # pVOptComma
  , "js-sources"                     # pVComma
  , "ld-options"                     # pVSpace
  , "mixins"                         # pFreeText -- XXX
  , "nhc98-options"                  # pVSpace
  , "other-extensions"               # pVOptComma
  , "other-languages"                # (VItem <$> pItem)
  , "other-modules"                  # pVOptComma
  , "pkg-config-depends"             # pVComma
  , "virtual-modules"                # pVComma
  --- library fields                 
  , "visibility"                     # (VItem <$> pItem)
  --- package fields                 
  , "author"                         # pFreeText
  , "bug-reports"                    # pFreeText
  , "build-type"                     # (VItem <$> pItem)
  , "cabal-version"                  # pFreeText -- (VRange <$> pVersionRange)
  , "category"                       # pFreeText
  , "copyright"                      # pFreeText
  , "data-dir"                       # pVSpace
  , "data-files"                     # pVOptComma
  , "description"                    # pFreeText
  , "extra-doc-files"                # pVOptComma
  , "extra-source-files"             # pVOptComma
  , "extra-tmp-files"                # pVComma
  , "homepage"                       # pFreeText
  , "license"                        # pFreeText
  , "license-file"                   # pVOptComma
  , "license-files"                  # pVOptComma
  , "maintainer"                     # pFreeText
  , "name"                           # (VItem <$> pItem)
  , "package-url"                    # pFreeText
  , "stability"                      # pFreeText
  , "subdir"                         # pFreeText
  , "synopsis"                       # pFreeText
  , "tested-with"                    # pFreeText
  , "version"                        # (VVersion <$> pVersion)
  -- test suite fields              
  , "main-is"                        # (VItem <$> pItem)
  , "test-module"                    # (VItem <$> pItem)
  , "type"                           # (VItem <$> pItem)
  -- source-repository fields
  , "location"                       # pFreeText
  -- flag fields
  , "manual"                         # (VBool <$> pBool)
  , "default"                        # (VBool <$> pBool)
  , "tag"                            # pFreeText
  ]
  where (#) = (,)
  -- XXX use local fixity
  
  
----------------------------------------------------------------------

-- XXX Wrong for strings
dropYAMLComments :: String -> String
dropYAMLComments [] = []
dropYAMLComments (c:cs) | c == '#' = dropYAMLComments (dropWhile (/= '\n') cs)
                        | otherwise = c : dropYAMLComments cs

pYAMLTop :: P YAMLValue
pYAMLTop = pYAMLRecord <* pWhite <* pChar end

pYAMLValue :: P YAMLValue
pYAMLValue =
      (YBool   <$> pBool)
  <|< (YInt    <$> pNumber)
--  <|< (YString <$> pString)
  <|< pYAMLArray
  <|< pYAMLRecord
  <|< (YString <$> pYAMLFree)

pYAMLArray :: P YAMLValue
pYAMLArray = do
  pWhite
  let
    pElem    = pChar '-' *> pSpaces *> pYAMLValue
    pElemFS  = pWhite *> pElem <* pFieldSep
    pElemsFS = esome pElemFS
    pElemNL  = pElem <* pChar '\n'
    pElemsNL = pFieldSep *> pChar '\n' *> esome pElemNL <* pushFieldSep
  YArray <$> (pElemsNL <|< pElemsFS)

pYAMLRecord :: P YAMLValue
pYAMLRecord = YRecord <$> esome pYAMLField

pYAMLFree :: P String
pYAMLFree = do
  pSpaces
  d <- nextToken
  guard (d /= '-')
  txt <- satisfyMany (\ c -> c /= end && c /= fieldSep && c /= '\n')
  pure txt

pYAMLField :: P (YAMLFieldName, YAMLValue)
pYAMLField = do
  pWhite
  pushColumn
  n <- pFieldName
  pColon
  v <- pYAMLValue
  pFieldSep
  pure (n, v)

----------------------------------------------------------------------

type Snapshot = (String, String)

pSnapshotsTop :: P [Snapshot]
pSnapshotsTop = pSnapshots <* pWhite <* pChar end

pSnapshots :: P [Snapshot]
pSnapshots = pWhite *> pChar '{' *> (esepBy pSnapshot (pWhite *> pChar ',')) <* pWhite <* pChar '}'

pSnapshot :: P Snapshot
pSnapshot = (,) <$> (pWhite *> pString) <*> (pWhite *> pChar ':' *> pWhite *> pString)

----------------------------------------------------------------------

readVersion :: String -> Version
readVersion = makeVersion . map read . words . map (\ c -> if c == '.' then ' ' else c)
