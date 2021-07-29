module Exercises where

dividedBy :: Integer -> Integer -> (Integer, Integer )
dividedBy num denom =
  go num denom 0 where
    go n d count 
      | n < d = (count, n)
      | otherwise = go (n - d) d (count +1)

data DivisionResult = Result (Integer, Integer) | DividedByZero deriving Show

div' :: Integer -> Integer -> DivisionResult
div' numerator denominator =
  go numerator denominator 0 where
    sameSign = (numerator < 0 && denominator < 0) || (numerator > 0 && denominator > 0) 
    go n d count 
      | d == 0 = DividedByZero
      | (abs n) < (abs d) && sameSign = Result (count, n)
      | (abs n) >= (abs d) && sameSign = go (n - d) d (count +1)
      | (abs n) >= (abs d) = go (n + d) d (count +1)
      | otherwise = Result (count, n)

cattyConny :: String -> String -> String
cattyConny x y = x ++ " mrow " ++ y

flippy = flip cattyConny

appedCatty = cattyConny "woops"

frappe = flippy "haha"

addFromTo :: Integral a => a -> a -> a
addFromTo from to =
  go from to 0 where
    go f t sum 
      | f == t = sum + f
      | otherwise = go (f + 1) t (sum + f)

multiplyEm :: Integral a => a -> a -> a
multiplyEm x y =
  go x y 0 where
    go x' y' total 
      | x' == 0 = 0
      | x' == 1 = total + y'
      | x' == -1 = total - y'
      | x' > 1 = go (x' - 1) y' (total + y')
      | x' < -1 = go (x' + 1) y' (total - y')

      
mult a b = go a b 0
    where go a b count
            | a == 0 = count
            | otherwise = go (a - 1) b (count + b)

multRecur :: Integral a => a -> a -> a
multRecur a 1 = a
multRecur a b = a + (multRecur a (b - 1))



mc91 :: Integral a => a -> a
mc91 x 
  | x > 100 = x - 10
  | otherwise = mc91 . mc91 $ x + 11
