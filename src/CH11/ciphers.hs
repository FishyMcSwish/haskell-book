module CH11.Ciphers where

import Data.Char

vigenereCipher :: String -> String -> String
vigenereCipher msg kw =
  map (uncurry shiftByChar) . zipWithSpaces (cycle kw) . map toUpper $ msg

zipWithSpaces :: String -> String -> [(Char, Char)]
zipWithSpaces [] _ = []
zipWithSpaces _ [] = []
zipWithSpaces (a:as) (b:bs) =
  if b == ' ' then
    (b, ' '): zipWithSpaces (a:as) bs
  else
    (b, a) : zipWithSpaces as bs 

shiftByChar :: Char -> Char -> Char
shiftByChar offsetChar char =
  let
    aIndex = ord 'A'
    offset= ord offsetChar - aIndex
    charIndex = ord char
  in
  if isAlpha char then 
    chr $ aIndex + mod (charIndex - aIndex + offset) 26 
  else 
    char


