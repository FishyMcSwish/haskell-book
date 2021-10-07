module CH22.ReaderPractice where

import Control.Applicative
import Data.Maybe

x = [1, 2, 3]
y = [4, 5, 6]
z = [7, 8, 9]

lookup' :: Eq a => a -> [(a, b)] -> Maybe b
lookup' _ [] = Nothing
lookup' key (x : xs) =
  if (fst x) == key
     then Just (snd x)
     else lookup' key xs

xs :: Maybe Integer
xs = lookup' 3 $ zip x y

ys :: Maybe Integer
ys = lookup' 6 $ zip y z

zs :: Maybe Integer
zs = lookup' 4 $ zip x z

z' :: Integer -> Maybe Integer
z' n = lookup' n $ zip x z 

x1 :: Maybe (Integer, Integer)
x1 = (,) <$> xs <*> ys

x2 :: Maybe (Integer, Integer)
x2 = (,) <$> ys <*> zs

x3 :: Integer -> (Maybe Integer, Maybe Integer)
x3 n =  (z' n , z' n)

summed :: Num c => (c, c) -> c
summed (x, y) = x + y

bolt :: Integer -> Bool
bolt = (&&) <$> (>3) <*> (<8)

run :: IO ()
run = do
  print $ sequenceA [Just 3, Just 2, Just 1]
  print $ sequenceA [x, y]
  print $ sequenceA [(>3), (<8), even] 7
