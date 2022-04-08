module Util where

-- pads out a given string on the right with the given character up to the given
-- length
padRight :: Int -> Char -> String -> String
padRight l c s = s ++ replicate (max 0 $ l - length s) c

padLeft :: Int -> Char -> String -> String
padLeft l c s = replicate (max 0 $ l - length s) c ++ s