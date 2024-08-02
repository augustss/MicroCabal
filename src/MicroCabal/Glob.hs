module MicroCabal.Glob(
  GlobPattern,
  --glob,
  listDirectoryRecursive,
  matchFiles,
  ) where
import Control.Exception
import Control.Monad
import System.Directory
import MicroCabal.Regex
import MicroCabal.Unix((</>))

-- A glob pattern can contain:
--  *   - any number of file name characters, except /
--  **  - any number of file name characters
--  X   - anything else is a character that just matches itself
type GlobPattern = String

{-
glob :: GlobPattern -> [String] -> [String]
glob p ss =
  let r = globToRegex p
  in  filter (regexMatch r) ss
-}

globToRegex :: GlobPattern -> Regex
globToRegex [] = eps
globToRegex ('*':'*':cs) = Star (Lit (Neg "")) `Seq` globToRegex cs
globToRegex ('*':cs) = Star (Lit (Neg "/")) `Seq` globToRegex cs
globToRegex (c:cs) = Lit (Pos [c]) `Seq` globToRegex cs

-- Recursively find all files in the given directory.
listDirectoryRecursive :: FilePath -> IO [FilePath]
listDirectoryRecursive ".git" = return []  -- Hack to avoid the gazillion files in .git/
listDirectoryRecursive x = do
  xs <- listDirectory x `catch` (\ (_ :: SomeException) -> return [])
  concat <$> (forM xs $ \ y -> (y:) <$> fmap (y </>) <$> listDirectoryRecursive (x </> y))

matchFiles :: FilePath -> [GlobPattern] -> IO [FilePath]
matchFiles dir pats = do
  fs <- listDirectoryRecursive dir
  let select pat =
        let re = globToRegex pat
        in  filter (regexMatch re) fs
  pure $ concatMap select pats
