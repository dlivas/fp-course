{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Course.FastAnagrams where

import Course.Core
import Course.List
import Course.Functor
import qualified Data.Set as S

-- Return all anagrams of the given string
-- that appear in the given dictionary file.
fastAnagrams ::
  Chars
  -> FilePath
  -> IO (List Chars)
fastAnagrams str fileName =
  perms <$> readFile fileName
  where
    uniques = listh . S.toList . S.fromList . hlist . words
    perms input =
      let
        fileWords = NoCaseString <$> uniques input
        anagramsList = NoCaseString <$> permutations str
        results = intersectBy (==) fileWords anagramsList
      in
        map ((++ ('\n' :. Nil)) . ncString) results

newtype NoCaseString =
  NoCaseString {
    ncString :: Chars
  }

instance Eq NoCaseString where
  (==) = (==) `on` map toLower . ncString

instance Show NoCaseString where
  show = show . ncString
