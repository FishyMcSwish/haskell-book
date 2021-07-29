module Exercises where

import Data.Bool
import Data.Char

eft :: (Enum a, Ord a) => a -> a -> [a]
eft start stop
        | start > stop = []
        | start == stop = [stop]
        | otherwise = eft (succ start) stop

eftBool :: Bool -> Bool -> [Bool]
eftBool x y = eft x y

makeWords :: String -> [String]
makeWords str =
  go str []
  where 
    go s l 
      | dropWhile (/= ' ') s == [] = l ++ [s]
      | otherwise = go (tail $ dropWhile ( /= ' ' ) s) (l ++ [takeWhile ( /= ' ' ) s])

makeWords' :: String -> [String]
makeWords' "" = []
makeWords' str =
  let 
    trimmed = dropWhile (== ' ') str
    nextWord = takeWhile (/= ' ') trimmed
    rest = dropWhile (/= ' ') trimmed
  in
  nextWord : makeWords' rest


firstSen = "Tyger Tyger, burning bright\n" 
secondSen = "In the forests of the night\n" 
thirdSen = "What immortal hand or eye\n" 
fourthSen = "Could frame thy fearful\
  \ symmetry?"
sentences = firstSen ++ secondSen ++ thirdSen ++ fourthSen

myLines :: String -> [String]
myLines "" = []
myLines str =
  let
    isNewLine = (==) '\n'
    trimmed = dropWhile isNewLine str
    thisLine = takeWhile (not . isNewLine) trimmed
    rest = dropWhile (not . isNewLine) trimmed
  in
    thisLine : (myLines rest)

shouldEqual =
  [ "Tyger Tyger, burning bright"
  , "In the forests of the night"
  , "What immortal hand or eye"
  , "Could frame thy fearful symmetry?" ]

splitBy :: Char -> String -> [String]
splitBy _ "" = []
splitBy char str=
  let
    isNewLine = (==) char
    trimmed = dropWhile isNewLine str
    thisLine = takeWhile (not . isNewLine) trimmed
    rest = dropWhile (not . isNewLine) trimmed
  in
    thisLine : (splitBy char rest)

myLines' = splitBy '\n'
makeWorks'' = splitBy ' '

itIsMystery xs =
  map (\x -> elem  x "aeiou") xs

mappy :: Integral a => [a] -> [a]
mappy xs =
  map (\x -> bool x (-x) $ x == 3) xs

--filtering

gimme3s :: Integral a => [a] -> [a]
gimme3s xs = 
  filter (\x -> mod x 3 == 0) xs

count3s =
  length . gimme3s

noMoreArticles :: String -> [String]
noMoreArticles str =
  filter (not . flip elem ["the", "a", "an"])  $ words str

zip' :: [a] -> [b] -> [(a, b)]
zip' [] _ = []
zip' _ [] =[]
zip' (a:as) (b:bs) = (a, b) : zip' as bs

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (a:as) (b:bs) = (f a b) : (zipWith' f as bs)

zip'' :: [a] -> [b] -> [(a, b)]
zip'' as bs =
  zipWith' (,) as bs

clearLowers :: String -> String
clearLowers  =
  filter isUpper 

capitalize :: String -> String
capitalize (x:xs) = toUpper x : xs

capitalizeAll :: String -> String
capitalizeAll [] = []
capitalizeAll (x:xs) = toUpper x : capitalizeAll xs

firstCapitalized :: String -> Char
firstCapitalized =
  toUpper . head


-- some valid fns

a = foldr  (flip const) 'a' [1..5]

