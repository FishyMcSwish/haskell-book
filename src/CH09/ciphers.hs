module Ciphers where

import Data.Char

caesar :: Int -> String -> String
caesar offset str = 
  map (shift offset) str

unCaesar :: Int -> String -> String 
unCaesar =
  caesar . negate

shift :: Int -> Char -> Char
shift int char
  | isUpper char = go int 'A' char
  | isLower char= go int 'a' char
  | otherwise = char
  where
    go :: Int -> Char -> Char -> Char
    go offset baseChar char = 
      chr $ shiftedIndex offset baseChar char

shiftedIndex offset baseChar char=
  ord baseChar + mod (distanceFromBaseChar offset baseChar char) 26
    
distanceFromBaseChar :: Int -> Char -> Char -> Int
distanceFromBaseChar offset baseChar char =
  ord char - ord baseChar + offset
