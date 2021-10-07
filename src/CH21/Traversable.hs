module CH21.Traversable where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

newtype Identity a = Identity a 
  deriving (Show, Eq, Ord)

instance Functor Identity where
  fmap f (Identity x) = Identity (f x)

instance Foldable Identity where
  foldr f acc (Identity x) = f x acc

instance Traversable Identity where
  traverse f (Identity x) = Identity <$> f x

instance (Arbitrary a) => Arbitrary (Identity a) where
  arbitrary = do
    a <- arbitrary
    return $ Identity a

instance (Eq a) => EqProp (Identity a) where
  (=-=) = eq

newtype Constant a b = Constant {getConstant :: a}
  deriving (Eq, Show)

instance Functor (Constant a)  where
  fmap _ (Constant x) = Constant x 

instance Foldable (Constant a) where
  foldr _ acc (Constant x) = acc

instance Traversable (Constant a) where
  traverse _ (Constant a) = pure (Constant a)

instance (Arbitrary a) => Arbitrary (Constant a b) where
  arbitrary = do 
    a <- arbitrary
    pure $ Constant a

instance (Eq a) => EqProp (Constant a b) where
  (=-=) = eq

data Optional a = Nada | Yep a
  deriving (Eq, Show)

instance Functor Optional where
  fmap _ Nada = Nada
  fmap f (Yep a) = Yep (f a)

instance Foldable Optional where
  foldr _ acc Nada = acc
  foldr f acc (Yep x) = f x acc 

instance Traversable Optional where
  traverse f Nada = pure Nada
  traverse f (Yep x) = Yep <$> f x


instance (Arbitrary a) => Arbitrary (Optional a) where
  arbitrary = do 
    a <- arbitrary
    elements [Nada, Yep a]

instance (Eq a) => EqProp (Optional a) where
  (=-=) = eq

data List a = Nil | Cons a (List a)

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons a as) = Cons (f a) (fmap f as)

instance Foldable List where
  foldr  _ acc Nil = acc
  foldr f acc (Cons x xs) = foldr f (f x acc) xs

instance Traversable List where
  traverse f Nil = pure Nil
  traverse f (Cons x xs) = 
    Cons <$> f x <*> traverse f xs


--arb and eqprop
--

data Bigger a b  = Bigger a b b b
  deriving (Eq, Show)

instance Functor (Bigger a) where
  fmap f (Bigger a b1 b2 b3) = Bigger a (f b1) (f b2) (f b3)

instance Foldable (Bigger a) where
  foldMap f (Bigger a x y z) =
    f x <> f y <> f z


instance Traversable (Bigger a) where
  traverse f (Bigger a x y z) = 
    Bigger a <$> f x <*> f y <*> f z


data S n a = S (n a) a deriving (Eq, Show)

instance (Functor n, Arbitrary (n a), Arbitrary a) => Arbitrary (S n a) where
  arbitrary = S <$> arbitrary <*> arbitrary

instance (Applicative n
         , Testable (n Property)
         , Eq a
         , Eq (n a)
         , EqProp a)
         =>  EqProp (S n a) where
           (=-=) = eq

instance Functor n => Functor (S n) where
  fmap f (S na a) = S (fmap f na) (f a)

instance Foldable n => Foldable (S n) where
  foldr f acc (S na a) = foldr f (f a acc) na 
  -- foldMap f (S n a) = foldMap f n <> f a

instance Traversable n => Traversable (S n) where
  traverse f (S n a) =
    S <$> traverse f n <*> f a
  

data Tree a =
  Empty |
    Leaf a |
      Node (Tree a) a (Tree a)
      deriving (Eq, Show)

instance Functor Tree where
  fmap f Empty = Empty
  fmap f (Leaf a) = Leaf $ f a
  fmap f (Node l v r) = Node (fmap f l) (f v) (fmap f r)

instance Foldable Tree where
  foldMap f Empty = mempty
  foldMap f (Leaf x) = f x
  foldMap f (Node l v r) = (foldMap f l) <> f v <> (foldMap f r)

instance (Arbitrary a) => Arbitrary (Tree a) where
  arbitrary = do 
    l <- arbitrary 
    r <- arbitrary
    v <- arbitrary
    elements [Leaf v, Empty, Node l v r]

instance Eq a => EqProp (Tree a) where
  (=-=) = eq

instance Traversable Tree where
  traverse _ Empty = pure Empty 
  traverse f (Leaf x) = Leaf <$> f x
  traverse f (Node l v r) = Node <$> traverse f l <*> f v <*> traverse f r
testTraverses = do
  quickBatch $ traversable (undefined :: Identity (Int, Char, String))
  quickBatch $ traversable (undefined :: Constant Int (Int, Char, String))
  quickBatch $ traversable (undefined :: Optional (Int, Char, String))
  quickBatch $ traversable (undefined :: S [] (Int, Char, String))
  quickBatch $ traversable (undefined :: Tree (Int, Char, String))

