{-# LANGUAGE NoMonomorphismRestriction #-}
module CH05.Exercises where

f1 = (* 9) 6
f2 = head [(0, "doge"), (1, "kitteh")]
f3 = head [(0 :: Integer, "Doge"), (1, "kitteh")]
f4 = if False then True else False  
f5 = length [1,3, 4, 5]
f6 = (( length [12,3]  )> ( length "Hats" ))

x1 = 5
y1 = x1 + 5
w1 = y1 * 10
f' = y1 / 4

x = "julie"
y = " <3 "
z = "Haskell"
f = x ++ y ++ z 

i :: a -> a
i a = a

c :: a -> b -> a
c a _ = a

c'' :: b -> a -> b
c'' b _ = b

c' :: a -> b -> b
c' _ b = b 

r :: [a] -> [a]
r as =
  as
  --tail as
  --drop 0 as
  --etc

co :: (b -> c) -> (a -> b) -> a -> c
co bTc aTb a =
  bTc $ aTb a

a :: (a -> c) -> a -> a
a _ x = x

a' :: (a -> b) -> a -> b
a' aTb a = aTb a
