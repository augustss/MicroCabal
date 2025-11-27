module MicroCabal.Normalize(normalize) where
import Data.Char(toLower)
import Data.Function
import Data.List
import Data.Maybe
import MicroCabal.Cabal
--import Debug.Trace

-- Do some normalization
--  * computre conditionals and flatten 'if/else'
--  * inline 'import'
--  * combine identical fields
--  * set library name

normalize :: FlagInfo -> Cabal -> Cabal
normalize info = libName . combine . inline . reduce info

combine :: Cabal -> Cabal
combine (Cabal ss) = Cabal $ map (\ (Section s n fs) -> Section s n (combineFields fs)) ss

combineFields :: [Field] -> [Field]
combineFields = map (foldl1 comb) . groupBy ((==) `on` fieldName) . sortBy (compare `on` fieldName)
  where fieldName (Field n _) = n
        fieldName _ = undefined -- Cannot happen, the Ifs are gone
        comb (Field n v1) (Field _ v2) = Field n (combineValue n v1 v2)
        comb _ _ = undefined

combineValue :: FieldName -> Value -> Value -> Value
combineValue _ (VItem x) (VItem y) | x == y = VItem x
combineValue _ (VItems xs) (VItems ys) = VItems (xs ++ ys)
combineValue _ (VBool x) (VBool y) = VBool (x && y)
combineValue _ (VPkgs xs) (VPkgs ys) = VPkgs (xs ++ ys)
combineValue _ (VXItem x) (VXItem y) = VXItem (x ++ "\n" ++ y)
combineValue n v1 v2 = error $ "fields " ++ show n ++ " cannot be combined, values=" ++ show (v1, v2)

inline :: Cabal -> Cabal
inline (Cabal ss) = Cabal (map sect nss)
  where (css, nss) = partition (\ (Section s _ _) -> s == "common") ss
        coms = [ (n, fs) | Section _ n fs <- css ]
        sect (Section s n fs) = Section s n $ concatMap inl fs
        inl (Field "import" (VItem n)) = fromMaybe (error $ "No common " ++ show n) $ lookup n coms
        inl f = [f]

libName :: Cabal -> Cabal
libName (Cabal []) = undefined
libName (Cabal (g@(Section _ _ gs):ss)) = Cabal $ g : map set ss
  where set (Section "library" "" fs) = Section "library" name fs
        set s = s
        name = getFieldString gs "name"

reduce :: FlagInfo -> Cabal -> Cabal
reduce info c = reduce' (addFlags c) c
  where addFlags (Cabal ss) = info{ flags = flags info ++ concatMap sect ss }
        sect (Section "flag" n fs) = [(n', dflt n' fs)]  where n' = map toLower n
        sect _ = []
        dflt n fs = head $ [ b | Field "default" (VBool b) <- fs ] ++ [error $ "no default for flag " ++ show n]

reduce' :: FlagInfo -> Cabal -> Cabal
reduce' info = mapField red
  where red (If c t e) --x | trace ("if " ++ show (c, cond info c)) False = undefined
                       | cond info c = concatMap red t
                       | otherwise   = concatMap red e
        red f = [f]

mapField :: (Field -> [Field]) -> Cabal -> Cabal
mapField f (Cabal ss) = Cabal (map sect ss)
  where
    sect (Section s n fs) = Section s n (concatMap f fs)

cond :: FlagInfo -> Cond -> Bool
cond info = eval
  where eval (CBool b) = b
        eval (Cand a b) = eval a && eval b
        eval (Cor  a b) = eval a || eval b
        eval (Cnot a)   = not (eval a)
        eval (Cos s)    = os info == s
        eval (Carch s)  = arch info == s
        eval (Cflag n)  = fromMaybe (error $ "Undefined flag " ++ show n) $ lookup (map toLower n) (flags info)
        eval (Cimpl s mv) = n == s && maybe True (inVersionRange v) mv  where (n, v) = impl info

inVersionRange :: Version -> VersionRange -> Bool
inVersionRange v (VEQ v') = v == v'
inVersionRange v (VGT v') = v >  v'
inVersionRange v (VLT v') = v <  v'
inVersionRange v (VLE v') = v <= v'
inVersionRange v (VGE v') = v >= v'
inVersionRange v (VOr vr1 vr2) = inVersionRange v vr1 || inVersionRange v vr2
inVersionRange v (VAnd vr1 vr2) = inVersionRange v vr1 && inVersionRange v vr2
inVersionRange v (VEQSet vs) = v `elem` vs
inVersionRange _ vr = error $ "inVersionRange: not implemented " ++ show vr
