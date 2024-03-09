module MicroCabal.Parse where
import Control.Applicative
import Control.Monad
import Data.Char
import Data.Maybe
import Text.ParserComb
import MicroCabal.Cabal
import Debug.Trace

type P a = Prsr LexState Char a

data LexState = LS Int [Int] [Char]
  deriving (Show)

initLexState :: [Char] -> LexState
initLexState cs = LS 0 [] cs

end :: Char
end = '\NUL'
fieldSep :: Char
fieldSep = '\FS'

instance TokenMachine LexState Char where
--  tmNextToken ls | trace ("tmNextToken: " ++ show ls) False = undefined
  tmNextToken ls@(LS _ [] []) = (end, ls)         -- ugly hack
  tmNextToken (LS i (_:ks) []) = (fieldSep, LS i ks [])
  tmNextToken (LS i [] (c:cs)) | c == '\n' = (c, LS     0 [] cs)
                               | otherwise = (c, LS (i+1) [] cs)
  tmNextToken (LS i kks@(k:ks) (c:cs)) | c /= '\n' = (c, LS i kks cs)
                                       | otherwise =
    let lead 0     _             = ('\n', LS 0 kks cs)              -- There are at least k leading spaces
        lead j (x:xs) | x == ' ' = lead (j-1) xs                    -- Count spaces
        lead _     _             = (fieldSep, LS 0 ks cs)                 -- Fewer than k spaces.  Generate FS, pop, and try again.
    in  lead (k+1) cs
  tmRawTokens (LS _ _ cs) = cs

pushColumn :: P ()
pushColumn = mapTokenState (\ (LS i ks cs) -> LS 0 (i:ks) cs)

{-
setCur :: LexState -> LexState
setCur (LS i Nothing cs) = LS i (Just i) cs
setCur _ = error "setCur"
-}

lower :: String -> String
lower = map toLower

-- Change lines with first non-space being '--' into just a newline
dropComments :: String -> String
dropComments = unlines . map cmt . lines
  where
    cmt s | take 2 (dropWhile (== ' ') s) == "--" = ""
          | otherwise = s

satisfySome :: String -> (Char -> Bool) -> P [Char]
satisfySome msg p = (:) <$> satisfy msg p <*> satisfyMany p

----------------------------------------------------------------

pTop :: P Cabal
pTop = pCabal <* pWhite <* pChar end

pCabal :: P Cabal
pCabal = Cabal <$> emany pField <*> emany pSection

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
  c <- satisfy "ident" isAlpha
  cs <- satisfyMany isIdent
  pure (c:cs)

pKeyWordNC :: String -> P ()
pKeyWordNC s = do
  i <- pIdent
  guard (s == lower i)

isIdent :: Char -> Bool
isIdent c = isAlphaNum c || c == '-' || c == '_' || c == '\'' || c == '.'

pNumber :: P String
pNumber = satisfySome "0..9" isDigit

pVersion :: P Version
pVersion = pSpaces *> (Version <$> esepBy1 pNumber pDot)

pVersionRange :: P VersionRange
pVersionRange = pVOr
  where
    pVOr  = foldr1 VOr  <$> esepBy1 pVAnd (pStr "&&")
    pVAnd = foldr1 VAnd <$> esepBy1 pVOp  (pStr "||")
    pVOp  = (pVOper <*> (pSpaces *> pVersion))
        <|< (pStr "(" *> pVersionRange <* pStr ")")
        <|< (pStr "=="  *> pVEq)
        <|< (pStr "^>=" *> pVGEHat)
    pVEq  = (VEQSet <$> pVSet) <|< do
      v <- pVersion
      (VEQWild v <$ pStr ".*") <|< pure (VEQ v)
    pVSet = (pStr "{" *> pCommaList pVersion <* pStr "}")
    pVGEHat = (VGEHat <$> pVersion) <|< (VGEHatSet <$> pVSet)

pVOper :: P (Version -> VersionRange)
pVOper = pSpaces *> choice [ VGT <$ pStr ">", VGT <$ pStr "<", VGT <$ pStr "<=", VGT <$ pStr ">="]

pStr :: String -> P ()
pStr s = pSpaces *> p s
  where p "" = pure ()
        p (c:cs) = pChar c *> p cs

pItem :: P Item
pItem = pSpaces *> (pString <|< pWord)

-- A string in quotation marks.
pString :: P Item
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

pCommaList :: P a -> P [a]
pCommaList p = (pStr "," *> esepBy1 p (pStr ","))
  <|< do
    xs <- esepBy p (pStr ",")
    when (not (null xs)) $
      () <$ optional (pStr ",")
    pure xs

pSpaceList :: P a -> P [a]
pSpaceList p = esepBy p pWhite

pOptCommaList :: P a -> P [a]
pOptCommaList p = pCommaList p <|< pSpaceList p

pVComma :: P Value
pVComma = VItems <$> pCommaList pItem

pVSpace :: P Value
pVSpace = VItems <$> pSpaceList pItem

pVOptComma :: P Value
pVOptComma = VItems <$> pOptCommaList pItem

pField :: P Field
pField = do
  pWhite
  pushColumn
  fn <- pFieldName
  if lower fn == "if" then do
    c <- pCond
    pNewLine
    t <- emany pField
    pFieldSep
    e <- do
      pWhite
      pushColumn
      pKeyWordNC "else"
      fs <- emany pField
      pFieldSep
      pure fs
     <|<
      pure []
    pure $ If c t e
   else do
    pColon
    let p = getParser fn
    v <- p
    pFieldSep
    pure $ Field fn v

pCond :: P String
pCond = satisfyMany (/= '\n')

pFreeText :: P Value
pFreeText = do
  s <- satisfyMany (\ c -> c /= end && c /= fieldSep)
  pure (VItem s)

pFieldName :: P FieldName
pFieldName = pIdent

pName :: P Name
pName = pWhite *> pIdent

pFields :: P [Field]
pFields = pSpaces *> pNewLine *> emany pField

pBool :: P Bool
pBool = (False <$ pKeyWordNC "false") <|< (True <$ pKeyWordNC "true")

pSection :: P Section
pSection = pWhite *> (
      Common     <$> (pKeyWordNC "common"     *>          pName) <*> pFields
  <|< Library    <$> (pKeyWordNC "library"    *> optional pName) <*> pFields
  <|< Executable <$> (pKeyWordNC "executable" *>          pName) <*> pFields
  <|< SourceRepo <$> (pKeyWordNC "source-repository" *>   pName) <*> pFields
      )

getParser :: FieldName -> P Value
getParser f = fromMaybe (error $ "Unknown field: " ++ f) $ lookup f parsers

parsers :: [(FieldName, P Value)]
parsers =
  [ "asm-options"                    # pVSpace
  , "asm-sources"                    # pVComma
  , "autogen-includes"               # pVOptComma
  , "autogen-modules"                # pVComma
  , "build-depends"                  # pFreeText -- XXX
  , "build-tool-depends"             # pVComma -- XXX
  , "build-tools"                    # pVComma -- XXX
  , "buildable"                      # (VBool <$> pBool)
  , "c-sources"                      # pVComma
  , "cc-options"                     # pVComma
  , "cmm-sources"                    # pVComma
  , "cmm-options"                    # pVComma
  , "cpp-options"                    # pVComma
  , "cxx-options"                    # pVComma
  , "default-extensions"             # pVOptComma
  , "default-language"               # (VItem <$> pItem)
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
  , "include-dirs"                   # pVOptComma
  , "includes"                       # pVOptComma
  , "install-includes"               # pVOptComma
  , "js-sources"                     # pVComma
  , "ld-options"                     # pVSpace
  , "mixins"                         # pFreeText -- XXX
  , "other-extensions"               # pVOptComma
  , "other-languages"                # (VItem <$> pItem)
  , "other-modules"                  # pVComma
  , "pkg-config-depends"             # pVComma
  , "virtual-modules"                # pVComma
  --- library fields                 
  , "visibility"                     # (VItem <$> pItem)
  --- package fields                 
  , "author"                         # pFreeText
  , "bug-reports"                    # pFreeText
  , "build-type"                     # (VItem <$> pItem)
  , "cabal-version"                  # (VVersion <$> pVersion)
  , "category"                       # pFreeText
  , "copyright"                      # pFreeText
  , "data-dir"                       # pVSpace
  , "data-files"                     # pVComma
  , "description"                    # pFreeText
  , "extra-doc-files"                # pVComma
  , "extra-source-files"             # pVComma
  , "extra-tmp-files"                # pVComma
  , "homepage"                       # pFreeText
  , "license"                        # pFreeText
  , "license-file"                   # pVOptComma
  , "maintainer"                     # pFreeText
  , "name"                           # (VItem <$> pItem)
  , "package-url"                    # pFreeText
  , "stability"                      # pFreeText
  , "synopsis"                       # pFreeText
  , "tested-with"                    # pVOptComma
  , "version"                        # (VVersion <$> pVersion)
  -- test suite  fields              
  , "main-is"                        # (VItem <$> pItem)
  , "test-module"                    # (VItem <$> pItem)
  , "type"                           # (VItem <$> pItem)
  ]
  where (#) = (,)
  -- XXX use local fixity
  
  
