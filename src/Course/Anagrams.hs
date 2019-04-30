{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Course.Anagrams where

import Course.Core
import Course.List
import Course.Functor

{-

Functions you will need
--
* fmap :: (a -> b) -> IO a -> IO b
* readFile :: FilePath -> IO Str
* lines :: Str -> [Str]
* permutations :: [a] -> [[a]]
* intersectBy :: (a -> a -> Bool) -> [a] -> [a] -> [a]
* toLower :: Char -> Char

Functions that might help
-
* on :: (b -> b -> c) -> (a -> b) -> a -> a -> c

-}

-- Return all anagrams of the given string
-- that appear in the given dictionary file.
anagrams ::
  Chars
  -> FilePath
  -> IO (List Chars)
anagrams str fileName =
  perms <$> readFile fileName
  where
    perms input =
      let
        fileWords = words input
        anagramsList = permutations str
        results = intersectBy equalIgnoringCase fileWords anagramsList
      in
        (++ ('\n' :. Nil)) <$> results
  -- error "todo: Course.Anagrams#anagrams"

-- Compare two strings for equality, ignoring case
equalIgnoringCase ::
  Chars
  -> Chars
  -> Bool
equalIgnoringCase =
  (==) `on` map toUpper
  -- toUpper <$> s == toUpper <$> s'
  -- error "todo: Course.Anagrams#equalIgnoringCase"
