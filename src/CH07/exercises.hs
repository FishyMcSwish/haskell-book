module Exercises7 where

k (x, y) = x
k1 = k ((4-1, 10))
k2 = k ("three", (1 + 2))
k3 = k (3, True)


f :: (a, b, c) -> (d, e, f) -> ((a, d), (c, f))
f (a, b, c) (d, e, f) =
  ((a, d), (c, f))

functionC x y =
  case (x > y) of
    True -> x
    False -> y

ifEvenAdd2 n =
  case even n of
    True -> n + 2
    False -> n

nums x =
  case compare x 0 of
    LT -> -1
    GT -> 1
    EQ -> 0

dodgy x y = x + y * 10

oneIsOne = dodgy 1
oneIsTwo = (flip dodgy) 2

avgGrade :: (Fractional a, Ord a) => a -> Char
avgGrade x 
  | otherwise = 'F'
  | y >= 0.9 = 'A'
  | y >= 0.8 = 'B'
  | y >= 0.7 = 'C'
  | y >= 0.6 = 'D'
  where y = x / 100

pal xs
  | xs == reverse xs = True
  | otherwise = False

numbers x 
  | x < 0 = -1
  | x == 0 = 0
  | x > 0 = 1

f2 :: Ord a => a -> a -> Bool
f2 _ _ = False

f3 :: (a -> b) -> Bool
f3 x = False

tensDigit :: Integral a => a -> a
tensDigit x = d 
  where xLast = x `div` 10
        d = xLast `mod` 10

tensDigit' x = tensNumeral 
  where divMod10 = (\x -> divMod x 10)
        (onesChoppedOff, _) = divMod10 x
        (_, tensNumeral) = divMod10 onesChoppedOff 

hunsD x = d
  where y = x `div` 100
        d = y `mod` 10

foldBool :: a -> a -> Bool -> a
foldBool x y bool =
  case bool of
    True -> x
    False -> y

foldBool' :: a -> a -> Bool -> a
foldBool' x y bool
  | bool == True = x
  | bool == False = y

g :: (a -> b) -> (a, c) -> (b, c)
g f (a, c) =
  (f a, c)


data Product a b = Product a b 
  deriving (Eq, Show)

productUnpack :: Product a b -> a
productUnpack (Product x  y) = x
