module Util where

-- | 'words' breaks a string up into a list of words, which were delimited
-- by white space.
split :: Char -> String -> [String]
split c s = case dropWhile ((==) c) s of
                 "" -> []
                 s' -> w : split c s''
                      where (w, s'') = break ((==) c) s'