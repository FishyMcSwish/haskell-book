module CH20.Foldable where

import Data.Monoid
import Data.Foldable

sumFM :: (Foldable t, Num a) => t a -> a
sumFM = 
   getSum . foldMap Sum


sumFoldR :: (Foldable t, Num a) => t a -> a
sumFoldR =
  foldr (+) 0


productFM :: (Foldable t, Num a) => t a -> a
productFM =
  getProduct . foldMap Product

productFoldR :: (Foldable t, Num a) => t a -> a
productFoldR =
  foldr (*) 1


elemFoldMap :: (Foldable t, Eq a) => a -> t a -> Bool
elemFoldMap x =
  getAny . foldMap (\a -> Any (a == x))

elemFoldR :: (Foldable t, Eq a) => a -> t a -> Bool
elemFoldR x =
  foldr  (\item acc -> item == x || acc) False

head' :: Foldable t => t a -> Maybe a
head' xs 
  | null xs = Nothing
  | otherwise = Just $ (head . toList) xs

minimumFoldR :: (Foldable t, Ord a) => t a -> Maybe a
minimumFoldR xs =
  head' xs >>= (\x -> pure $ foldr min x xs) 
  -- case head' xs of
  --   Nothing -> Nothing
  --   Just x ->
  --     Just $ foldr min x xs

minimumFoldMap :: (Foldable t, Ord a) => t a -> Maybe a
minimumFoldMap xs = 
  getMin $ foldMap (Min . Just) xs

newtype Min a = Min {getMin :: Maybe a}

instance (Ord a) => Semigroup (Min a) where
  Min Nothing <> x = x
  x <> Min Nothing = x
  Min (Just x) <> Min (Just y) = Min $ Just (min x y)

instance (Ord a) => Monoid (Min a) where
  mempty = Min Nothing


null' :: (Foldable t) => t a -> Bool
null' xs = length xs == 0

length' :: (Foldable t) => t a -> Int
length' xs = 
  foldr (\_ y -> y + 1) 0 xs

toList' :: (Foldable t) => t a -> [a]
toList' = foldr (:) [] 

toList'' :: (Foldable t) => t a -> [a]
toList'' = 
  foldMap pure 

fold' :: (Foldable t, Monoid m) => t m -> m
fold' =
  foldMap id

foldMap'' :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
foldMap'' f xs =
  foldr (\item acc -> f item <> acc ) mempty xs

data Const' a b = Const' a

instance Foldable (Const' a) where
  foldr f acc (Const' x) = acc

data Two a b = Two a b

instance Foldable (Two a) where
  foldr f acc (Two x y) = f y acc

data Three a b c = Three a b c

instance Foldable (Three a b) where
  foldr f acc (Three _ _ c) = f c acc

data Three' a b = Three' a b b

instance Foldable (Three' a) where
  foldr f acc (Three' _ x y) =  f x . f y $ acc
  foldMap f (Three' _ x y) = f x <> f y

data Four' a b  = Four' a b b b

instance Foldable (Four' a) where
  foldr f acc (Four' _ x y z) = f x . f y . f z $ acc
  foldMap f (Four' _ x y z) = f x <> f y <> f z

filterF :: (Applicative f, Foldable t, Monoid (f a)) => (a -> Bool) -> t a -> f a
filterF f xs = 
  foldMap (\x -> if f x then pure x else mempty) xs

