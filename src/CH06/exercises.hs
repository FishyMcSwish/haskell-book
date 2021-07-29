module CH06.Exercises where

import Data.List

data Trivial = Trivial'

instance Eq Trivial where
  Trivial' == Trivial' = True


data WithConstraint a = WithConstraint a

instance Eq a => Eq (WithConstraint a) where
  (==) (WithConstraint a) (WithConstraint a') = a == a'


data Which a = This a | That a

instance Eq a => Eq (Which a) where
  (==) (This a) (This a') = a == a'
  (==) (That a) (That a') = a == a'
  (==) _ _ = False

data EitherOr a b = 
  Hello a 
  | Goodbye b

instance (Eq a, Eq b) => Eq (EitherOr a b) where
  (==) (Hello x) (Hello x') = x == x'
  (==) (Goodbye x) (Goodbye x') = x == x'
  
add = (+) :: Integer -> Integer -> Integer


class Numberish a where
  fromNumber :: Integer -> a
  toNumber :: a -> Integer
  defaultNumber :: a

newtype Age = Age Integer deriving (Eq, Show)

instance Numberish Age where
  fromNumber n = Age n
  toNumber (Age n) = n
  defaultNumber = Age 65 

newtype Year = Year Integer deriving (Eq, Show)

instance Numberish Year where
  fromNumber n = Year n
  toNumber (Year n) = n
  defaultNumber = Year 1988 

--defaultNumber won't work unless we provie a concrete type!  
-- you have to provide defaultNumber :: Age or defaultNumber :: Year


data Mood = Woot | Blah deriving ( Show, Eq)
settleDown x = if x == Woot then Blah else x


data Rocks = 
  Rocks String deriving (Eq, Show)

data Yeah = 
  Yeah Bool deriving (Eq, Show)

data Papu =
  Papu Rocks Yeah deriving (Eq, Show)

equalityForAll :: Papu -> Papu -> Bool
equalityForAll p p' = p == p'


i:: Num a => a
i = 1

f :: Float
f = 1.0


fraud' :: a -> a
fraud' x = x

myX = 1 :: Int

sigmund:: Int -> Int 
sigmund x = myX

jung :: Ord a => [a] -> a
jung xs = head (sort xs ) 

young :: [Char] -> Char
young xs = head (sort xs)

mySort :: [Char] -> [Char]
mySort = sort

signifier :: [Char] -> Char
signifier xs = head (mySort xs)

chk :: Eq b => (a -> b) -> a -> b -> Bool
chk fn a' b' =
  b == (fn a)

arith :: Num b => (a -> b) -> Integer -> a -> b
arith fn int a =
  fn a
