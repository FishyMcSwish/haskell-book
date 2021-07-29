module CH17.Exercises where

import Control.Applicative
import Data.List (elemIndex)
import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

added :: Maybe Integer
added = 
   (+3) <$> (lookup 3 $ zip [1, 2, 3] [4, 5, 6])



y :: Maybe Integer
y = lookup 3 $ zip [1, 2, 3] [4, 5, 6]

z :: Maybe Integer
z = lookup 2 $ zip [1, 2, 3] [4, 5, 6]

tupled :: Maybe (Integer, Integer)
tupled = (,) <$> y <*> z


x' :: Maybe Int
x' = elemIndex 4 [1, 2, 3, 4, 5, 6]

y' :: Maybe Int
y' = elemIndex 4 [1, 2, 3, 4, 5, 6]

max' :: Int -> Int -> Int
max' = max

maxed :: Maybe Int
maxed = max' <$> x' <*> y'



xs = [1, 2, 3]
ys = [4, 5, 6]

x'' :: Maybe Integer
x'' = lookup 3 $ zip xs  ys

y'' :: Maybe Integer
y'' = lookup 2 $ zip xs  ys

summed :: Maybe Integer
summed = (<$>) sum $ (,) <$> x'' <*> y''


newtype Constant a b = Constant {getConstant :: a}
  deriving (Eq, Ord, Show)

instance Functor (Constant a) where
  fmap _ (Constant x) = Constant x

instance (Monoid a) => Applicative (Constant a) where
  pure _ = Constant mempty
  (Constant x) <*> (Constant y) = Constant (x <> y)


-- fixer upper exercise
--
fn1 = const <$> Just "Hello" <*> pure "World"
fn2 = (,,,) <$> Just 90 <*> Just 10 <*> Just "Tierness" <*> pure [1, 2, 3]


-- List applicative exercises

data List a = Nil | Cons a (List a) deriving (Eq)

instance (Show a) => Show (List a) where
  show Nil = "Nil"
  show (Cons x xs) = show x ++ "-" ++ show xs

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons x (xs)) = Cons (f x) (fmap f xs)

append :: List a -> List a -> List a
append Nil ys = ys
append (Cons x xs) ys = Cons x $ xs `append` ys

instance Applicative List where
  pure x = Cons x Nil
  Nil <*> _ = Nil
  _ <*> Nil = Nil
  (Cons f fs) <*> xs = (fmap f xs) `append` (fs <*> xs)

testFnList = Cons (+1) (Cons  (*2) Nil)
testValList = Cons 1 (Cons 2 Nil)

instance Semigroup (List a) where
  x <> y = x `append` y

instance Monoid (List a) where 
  mempty = Nil

instance (Arbitrary a) => Arbitrary (List a) where
  arbitrary =
    frequency [(1, return Nil), (4, Cons <$> arbitrary <*> arbitrary)]

--alternative list generator
listGen :: Arbitrary a => Gen (List a)
listGen = do
  a <- arbitrary
  l <- listGen
  frequency [(1, return Nil), (4, return $ Cons a l)]

instance Eq a => EqProp (List a) where
  (=-=) = eq

testApplicative :: IO ()
testApplicative = do
  quickBatch $ applicative (undefined :: List (Int, String, Char))
  quickBatch $ applicative (undefined :: ZipList' (Int, String, Char))

testMonoid :: IO ()
testMonoid = do
  quickBatch $ monoid (undefined :: List String)
  quickBatch $ monoid (undefined :: ZipList' String)

fold :: (a -> b -> b) -> b -> List a -> b
fold _ b Nil = b
fold f b (Cons h t) = f h (fold f b t)

concat' :: List (List a) -> List a
concat' = fold append Nil

flatMap :: (a -> List b) -> List a -> List b
flatMap f as = 
  concat' $ f <$> as 

take' :: Int -> List a -> List a
take' _ Nil = Nil
take' n (Cons x xs) 
  | n > 0 = Cons x (take' (n-1) xs)
  | otherwise = Nil


repeat' :: a -> List a 
repeat' x =
  Cons x (repeat' x)

toMyList :: [a] -> List a
toMyList = foldr Cons Nil



--ZipList

newtype ZipList' a = ZipList' (List a) deriving (Eq, Show)

instance Eq a => EqProp (ZipList' a) where
  xs =-= ys =  xs' `eq` ys'
    where xs' = let (ZipList' l) = xs 
                in take' 3000 l
          ys' = let (ZipList' l) = ys 
                in take' 3000 l

instance Functor ZipList' where
  fmap f (ZipList' xs) = ZipList' (fmap f xs)

  {-
instance Semigroup a => Semigroup (ZipList' a) where
  (ZipList' Nil) <> _ = ZipList' Nil
  _ <> (ZipList' Nil) = ZipList' Nil
  (ZipList' (Cons x xs)) <> (ZipList' (Cons y ys)) = ZipList' (Cons (x <> y) (xs <> ys))
-}

fixed :: Semigroup a => ZipList' a -> ZipList' a -> ZipList' a
fixed (ZipList' Nil) _ = ZipList' Nil
fixed _ (ZipList' Nil) = ZipList' Nil
fixed (ZipList' (Cons x xs)) (ZipList' (Cons y ys)) = ZipList' (Cons (x <> y) rest)
                                                        where (ZipList' rest) = ZipList' xs <> ZipList' ys

instance Semigroup a => Semigroup (ZipList' a) where
  --(<>) = liftA2 (<>)
  (<>) zl1 zl2 = (<>) <$> zl1 <*> zl2 

instance Monoid a => Monoid (ZipList' a) where
  mempty = pure mempty
  
zipApply :: List (a-> b) -> List a -> List b
zipApply Nil _ = Nil
zipApply _ Nil = Nil
zipApply (Cons f fs) (Cons x xs) = 
          Cons (f x) (zipApply fs xs)

      
instance Applicative ZipList' where
  pure x = ZipList' (repeat' x)
  (ZipList' fs) <*> (ZipList' xs) = ZipList' (zipApply fs xs)
    {-
instance Applicative ZipList' where
  pure x = ZipList' (repeat' x)
  (ZipList' Nil) <*> _ = ZipList' Nil
  _ <*> (ZipList' Nil) = ZipList' Nil
  (ZipList' (Cons f fs)) <*> (ZipList' (Cons x xs)) =
    ZipList' (Cons (f x) Nil) <> (ZipList' fs <*> ZipList' xs)
-}
instance (Arbitrary a) => Arbitrary (ZipList' a) where
  arbitrary =
    ZipList' <$> listGen



zl' = ZipList'
z1 = zl' $ toMyList [(+9), (*2), (+8)]
z2 = zl' $ toMyList [1..3]
r1 = z1 <*> z2

z3 = pure 1
r2 = z1 <*> z3

z4 = zl' $ toMyList [1, 2]

r3 = pure id <*> z4


zWords = ZipList' $ toMyList ["asdf", "afs" , "vasddvas"]
zwords1 = ZipList' $ toMyList ["a"]
zNil = ZipList' Nil


data Validation e a = Fail e | Suc a deriving (Eq, Show)

instance Functor (Validation e) where
  fmap _ (Fail x) = Fail x
  fmap f (Suc a) = Suc (f a)

instance Monoid e => Applicative (Validation e) where
  pure = Suc 
  (Fail x) <*> (Fail y) = Fail $ x <> y
  (Fail x ) <*> _ = Fail x
  _ <*> (Fail x) = Fail x
  (Suc f) <*> (Suc x) = Suc (f x)

-- The monad instance doesn't work, we only have access to one Fail at a time.
-- This won't compile
-- instance Monoid e => Monad (Validation e) where
--   return = pure
--   (Fail x) >>= f =
--     case f x of
--       (Suc y) -> Suc y
--       (Fail y) -> Fail (x <> y) <-- wont type check, f needs an 'a' from a
--      -- Suc, not an 'e' from a fail
--   (Suc x) >>= f = f x

instance (Arbitrary a, Arbitrary e) => Arbitrary (Validation e a) where
  arbitrary = do
    a <- arbitrary
    e <- arbitrary
    frequency [(1, return $ Fail e), (2, return $ Suc a)]

instance (Eq e, Eq a) => EqProp (Validation e a) where
  (=-=) = eq

  

  

testValidation :: IO ()
testValidation = do
  quickBatch $ applicative (undefined :: Validation String (Int, String, Char))


--Type exercises
pureList :: a -> [a]
pureList = pure

applyList :: [(a ->b)] -> [a] -> [b]
applyList = (<*>)

pureIO :: a -> IO a
pureIO = pure

applyIO :: IO (a -> b) -> IO a -> IO b
applyIO = (<*>)

pureTuple :: (Monoid b) => a -> (b, a)
pureTuple = pure

applyTuple ::(Monoid c) =>  (c, a -> b) -> (c, a) -> (c, b)
applyTuple = (<*>)

pureFn :: a -> (e -> a) 
pureFn = pure

applyFn :: (e -> (a -> b)) -> (e -> a) -> (e -> b)
applyFn = (<*>)


data Pair a = Pair a a deriving (Eq, Show)

instance Functor Pair where 
  fmap f (Pair x y) = Pair (f x) (f y)

instance Applicative Pair where
  pure x = Pair x x
  (Pair f g) <*> (Pair x y) = Pair (f x) (g y)

instance (Arbitrary a) => Arbitrary (Pair a) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary 
    return $ Pair x y

instance (Eq a) => EqProp (Pair a) where
  (=-=) = eq

data Two a b = Two a b deriving (Eq, Show)

instance Functor (Two a) where
  fmap f (Two x y) = Two x (f y)

instance (Monoid a) => Applicative (Two a) where
  pure x = Two mempty x
  (Two a1 f) <*> (Two a2 x) = Two (a1 <> a2) (f x)

instance (Eq a, Eq b) => EqProp (Two a b) where
  (=-=) = eq

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    a <- arbitrary 
    b <- arbitrary 
    return $ Two a b

data Three a b c = Three a b c deriving (Eq, Show)

instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b (f c)

instance (Monoid a, Monoid b) => Applicative (Three a b) where
  pure x = Three mempty mempty x
  (Three a1 b1 f) <*> (Three a2 b2 x) = Three (a1 <> a2) (b1 <> b2) (f x)

instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where
  (=-=) = eq

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = do
    a <- arbitrary 
    b <- arbitrary 
    c <- arbitrary 
    return $ Three a b c


data Three' a b = Three' a b b deriving (Eq, Show)

instance  Functor (Three' a) where
  fmap f (Three' a x y) = Three' a (f x) (f y)

instance (Monoid a) => Applicative (Three' a) where
  pure x = Three' mempty x x
  (Three' a f g) <*> (Three' a' x y) = Three' (a <> a') (f x) (g y)

instance (Eq a, Eq b) => EqProp (Three' a b) where
  (=-=) = eq

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
  arbitrary = do
    a <- arbitrary 
    b1 <- arbitrary 
    b2 <- arbitrary 
    return $ Three' a b1 b2

data Four a b c d = Four a b c d deriving (Eq, Show)

instance Functor (Four a b c) where
  fmap f (Four a b c d) = Four a b c (f d)

instance (Monoid a, Monoid b, Monoid c) => Applicative (Four a b c) where
  pure x = Four mempty mempty mempty x 
  (Four a b c f) <*> (Four a' b' c' x) = Four (a <> a') (b <> b') (c <> c') (f x)

instance (Eq a, Eq b, Eq c, Eq d) => EqProp (Four a b c d) where
  (=-=) = eq

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Arbitrary (Four a b c d) where
  arbitrary = do
    a <- arbitrary 
    b <- arbitrary 
    c <- arbitrary 
    d <- arbitrary 
    return $ Four a b c d

data Four' a b = Four' a a a b deriving (Eq, Show)

instance Functor (Four' a) where
  fmap f (Four' a1 a2 a3 x) = Four' a1 a2 a3 (f x) 

instance (Monoid a) => Applicative (Four' a) where
  pure x = Four' mempty mempty mempty x
  (Four' a b c f) <*> (Four' a' b' c' x) = Four' (a <> a') (b <> b') (c <> c') (f x)


instance (Eq a, Eq b) => EqProp (Four' a b) where
  (=-=) = eq

instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
  arbitrary = do
    a <- arbitrary 
    b <- arbitrary 
    c <- arbitrary 
    d <- arbitrary 
    return $ Four' a b c d

testExerciseTypes :: IO ()
testExerciseTypes = do
  quickBatch $ applicative (undefined :: Pair (Int, String, Char))
  quickBatch $ applicative (undefined :: Two String (Int, String, Char))
  quickBatch $ applicative (undefined :: Three [Int] String (Int, String, Char))
  quickBatch $ applicative (undefined :: Three' String (Int, String, Char))
  quickBatch $ applicative (undefined :: Four (Maybe String) [Int] String (Int, String, Char))
  quickBatch $ applicative (undefined :: Four' String (Int, String, Char))


--Combinations
stops :: String
stops = "pbtdkg"

vowels :: String
vowels = "aeiou"

combos :: [a] -> [b] -> [c] -> [(a, b, c)]
combos =
  liftA3  (,,) 



class Monoid' m
  where
  smoosh :: [m] -> m
-- Laws
-- Smooshing a singleton list gives you back the only element
-- smoosh [v] = v
-- Smooshing all the elements of a list of lists gives you the same result as smooshing each list, then smooshing the results
-- smoosh (concat xss) = smoosh (smoosh <$> xss)
--
