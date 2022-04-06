module Util where

-- pads out a given string on the right with the given character up to the given
-- length
padRight :: String -> Int -> Char -> String
padRight s l c = s ++ replicate (max 0 $ l - length s) c