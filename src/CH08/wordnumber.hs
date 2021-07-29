module WordNumber where

import Data.List (intersperse)

--probably just do a guard or case here
digitToWord :: Int -> String
digitToWord 0 = "zero"
digitToWord 1 = "one"
digitToWord 2 = "two"
digitToWord 3 = "three"
digitToWord 4 = "four"
digitToWord 5 = "five"
digitToWord 6 = "six"
digitToWord 7 = "seven"
digitToWord 8 = "eight"
digitToWord 9 = "nine"
digitToWord _ = "error"

-- i like this pattern match cuz it really is exhaustive
-- would be more terse with a guard tho
-- for some reason i trust pattern matches more than guards?
digits :: Int -> [Int]
digits n 
  | n >= 10 = (digits $ div n 10) ++ [mod n 10] 
  | otherwise = [n]

wordNumber :: Int -> String
wordNumber = foldr (++) "" . intersperse "-" . map digitToWord . digits
