-- Originally stolen from https://crypto.stanford.edu/~blynn/haskell/re.html

-- Regular expression matching using Brzozowski's algorithm
module MicroCabal.Regex(CharClass(..), Regex(..), eps, regexMatch) where
import Data.List(sort, nub)

data CharClass = Pos String | Neg String
  deriving (Eq, Ord, Show)

elemCC :: Char -> CharClass -> Bool
elemCC c (Pos cs) = c `elem` cs
elemCC c (Neg cs) = c `notElem` cs

data Regex
  = Lit CharClass
  | Seq Regex Regex
  | Star Regex
  | Or [Regex]
  | And [Regex]
  | Not Regex
  deriving (Eq, Ord, Show)

regexMatch :: Regex -> String -> Bool
regexMatch re ""    = nullable re
regexMatch re (c:s) = regexMatch (derive c re) s

-- The regex `()`. The language containing only the empty string.
eps :: Regex
eps = Star noGood

-- The regex `[]`. The empty language.
noGood :: Regex
noGood = Lit $ Pos []

-- The regex `.*`. The language containing everything.
allGood :: Regex
allGood = Star $ Lit $ Neg []

nullable :: Regex -> Bool
nullable re =
  case re of
    Lit _   -> False
    Star _  -> True
    Seq r s -> nullable r && nullable s
    Or  rs  -> any nullable rs
    And rs  -> all nullable rs
    Not r   -> not $ nullable r

derive :: Char -> Regex -> Regex
derive c re =
  case re of
    Lit cc | elemCC c cc   -> eps
           | otherwise     -> noGood
    Star r                 -> derive c r `mkSeq` mkStar r
    r `Seq` s | nullable r -> mkOr [derive c r `mkSeq` s, derive c s]
              | otherwise  -> derive c r `mkSeq` s
    And rs                 -> mkAnd $ map (derive c) rs
    Or  rs                 -> mkOr  $ map (derive c) rs
    Not r                  -> mkNot $ derive c r

-- Smart constructors
mkSeq :: Regex -> Regex -> Regex
mkSeq r s
  | r == noGood || s == noGood = noGood
  | r == eps       = s
  | s == eps       = r
  | x `Seq` y <- r = x `mkSeq` (y `mkSeq` s)
  | otherwise      = r `Seq` s

mkOr :: [Regex] -> Regex
mkOr xs
  | allGood `elem` zs = allGood
  | null zs           = noGood
  | [z] <- zs         = z
  | otherwise         = Or zs
  where
    zs = nub $ sort $ filter (/= noGood) flat
    flat           = concatMap deOr xs
    deOr (Or rs)   = rs
    deOr r         = [r]

mkAnd :: [Regex] -> Regex
mkAnd xs
  | noGood `elem` zs = noGood
  | null zs          = allGood
  | [z] <- zs        = z
  | otherwise        = And zs
  where
    zs = nub $ sort $ filter (/= allGood) flat
    flat             = concatMap deAnd xs
    deAnd (And rs)   = rs
    deAnd r          = [r]

mkStar :: Regex -> Regex
mkStar (Star s) = mkStar s
mkStar r        = Star r

mkNot :: Regex -> Regex
mkNot (Lit (Pos [])) = allGood
mkNot (Not s)        = s
mkNot r              = Not r
