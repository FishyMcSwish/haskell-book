module StandardFunctions where

myOr :: [Bool] -> Bool
myOr [] = False
myOr (x:xs) = x || myOr xs

myAny :: (a -> Bool) -> [ a ] -> Bool
myAny _ [] = False
myAny f (a:as) = if f a then True else myAny f as

myElem :: Eq a => a -> [a] -> Bool
myElem _ [] = False
myElem x (y:ys) = if x == y then True else myElem x ys

myElem' :: Eq a => a -> [a] -> Bool
myElem' x ys = myAny (== x) ys

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]

squish :: [[a]] -> [a]
squish list = foldr (++) [] list

squish' :: [[a]] -> [a]
squish' [] = []
squish' (x:xs) = x ++ squish' xs

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f =
  squish . map f

squishAgain :: [[a]] -> [a]
squishAgain =
  squishMap id

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy _ [x] = x
myMaximumBy f (x:xs) = case f x maxOfRest of
                         GT -> x
                         EQ -> x
                         LT -> maxOfRest
                       where maxOfRest = myMaximumBy f xs
      
myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy _ [x] = x
myMinimumBy f (x:xs) = case f x minOfRest of
                         GT -> minOfRest
                         EQ -> x
                         LT -> x
                       where minOfRest = myMinimumBy f xs
      
myMaximum :: (Ord a ) => [a] -> a
myMaximum  = 
  myMaximumBy compare 

myMinimum :: (Ord a ) => [a] -> a
myMinimum = myMinimumBy compare 
