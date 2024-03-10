module MicroCabal.Normalize(normalize) where
import Data.Maybe
import MicroCabal.Cabal

-- Do 3 things:
--  * computre conditionals and flatten 'if/else'
--  * inline 'import'
--  * combine identical fields

normalize :: FlagInfo -> Cabal -> Cabal
normalize info = combine . inline . reduce info

combine :: Cabal -> Cabal
combine c = c

inline :: Cabal -> Cabal
inline c = c

reduce :: FlagInfo -> Cabal -> Cabal
reduce info = mapField red
  where red (If c t e) | cond info c = concatMap red t
                       | otherwise   = concatMap red e
        red f = [f]

mapField :: (Field -> [Field]) -> Cabal -> Cabal
mapField f (Cabal gfs ss) = Cabal (concatMap f gfs) (map sect ss)
  where
    sect (Common     n fs) = Common     n (concatMap f fs)
    sect (Library    n fs) = Library    n (concatMap f fs)
    sect (Executable n fs) = Executable n (concatMap f fs)
    sect (SourceRepo n fs) = SourceRepo n (concatMap f fs)

cond :: FlagInfo -> Cond -> Bool
cond info = eval
  where eval (CBool b) = b
        eval (Cand a b) = eval a && eval b
        eval (Cor  a b) = eval a || eval b
        eval (Cnot a)   = not (eval a)
        eval (Cos s)    = os info == s
        eval (Carch s)  = arch info == s
        eval (Cflag n)  = fromMaybe (error $ "Undefined flag " ++ show n) $ lookup n (flags info)
        eval (Cimpl s mv) = n == s && maybe True (inVersionRange v) mv  where (n, v) = impl info

inVersionRange :: Version -> VersionRange -> Bool
inVersionRange _ _ = True  -- XXX
