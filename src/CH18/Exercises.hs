module CH18.Exercises where

import Control.Monad
import CH17.Exercises
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

bind :: Monad m => (a -> m b) -> m a -> m b
bind f x = 
  join . fmap f $ x


twiceWhenEven :: [Int] -> [Int]
twiceWhenEven xs = do
  x <- xs
  if even x
     then [x * x, x * x]
     else [x * x]

twiceWhenEven' :: [Int] -> [Int]
twiceWhenEven' xs =
  xs >>= (\x -> if even x
            then [x * x, x * x]
            else [x * x]) 

twiceWhenEvenOrNothing :: [Int] -> [Int]
twiceWhenEvenOrNothing xs = do
  x <- xs
  if even x
     then [x * x, x * x]
     else []
     

-- playing with cows
--

data Cow = Cow {
                 name :: String
               , age :: Int
               , weight :: Int
               } deriving (Eq, Show)

noEmpty :: String -> Maybe String
noEmpty "" = Nothing
noEmpty str = Just str

noNegative :: Int -> Maybe Int
noNegative n | n >= 0 = Just n
             | otherwise = Nothing

weightCheck :: Cow -> Maybe Cow
weightCheck c =
  let w = weight c
      n = name c
  in if n == "Bess" && w > 499
        then Nothing
        else Just c

mkSphericalCowDo :: String -> Int -> Int -> Maybe Cow
mkSphericalCowDo name' age' weight' = do
  nammy <- noEmpty name'
  agey <- noNegative age'
  weighty <- noNegative weight'
  weightCheck (Cow nammy agey weighty)

mkSphericalCowBind :: String -> Int -> Int -> Maybe Cow
mkSphericalCowBind name' age' weight' =
  noEmpty name' >>=
    \nammy -> 
      noNegative age' >>=
        \agey -> noNegative weight' >>= 
          \weighty ->
            weightCheck (Cow nammy agey weighty)


mkSphericalCowBind' :: String -> Int -> Int -> Maybe Cow
mkSphericalCowBind' name' age' weight' =
  noEmpty name' >>=
    firstCall age' weight'


firstCall :: Int -> Int -> (String -> Maybe Cow)
firstCall age' weight' = 
    \nammy -> 
      noNegative age' >>=
        \agey -> 
          noNegative weight' >>= 
            lastCall nammy agey


lastCall :: String -> Int -> (Int -> Maybe Cow)
lastCall nammy agey =
          \weighty ->
            weightCheck (Cow nammy agey weighty)

-- playing with Either

type Founded = Int
type Coders = Int
data SoftwareShop = 
  Shop {
        founded :: Founded
       , programmers :: Coders
       } deriving (Eq, Show)

data FoundedError =
  NegativeYears Founded
    | TooManyYears Founded
    | NegativeCoders Coders
    | TooManyCoders Coders
    | TooManyCodersForYears Founded Coders
    deriving (Eq, Show)

validateFounded :: Int -> Either FoundedError Founded
validateFounded x 
  | x < 0 = Left $ NegativeYears x
  | x > 500 = Left $ TooManyYears x
  | otherwise = Right x

validateCoders :: Int -> Either FoundedError Coders
validateCoders x 
  | x < 0 = Left $ NegativeCoders x
  | x > 5000 = Left $ TooManyCoders x
  | otherwise = Right x

mkSoftware :: Int -> Int -> Either FoundedError SoftwareShop
mkSoftware years coders = do
  founded <- validateFounded years
  coders <- validateCoders coders
  if coders > div founded 10
     then Left $ TooManyCodersForYears founded coders
     else Right $ Shop founded coders

ap' :: (Monad m) => m (a -> b) -> m a -> m b
ap' m m' = do
 x <- m
 x' <- m'
 return (x x')

ap'' :: (Monad m) => m (a -> b) -> m a -> m b
ap'' m m' =
  m >>= 
    \x -> 
      m' >>=
        \x' ->
          return (x x')


data Sum a b = First a | Second b deriving (Eq, Show)

instance Functor (Sum a ) where 
  fmap _ (First x) = First x
  fmap f (Second b) = Second (f b)

instance Applicative (Sum a) where
  pure x = Second x
  (First x) <*> _ = First x
  _ <*> (First x) = First x
  (Second f) <*> (Second x) = Second (f x)

instance Monad (Sum a) where
  return = pure
  (First x) >>= _ = First x
  (Second x) >>= f  = f x


data S a = S a deriving (Eq, Show)

instance Num a => Semigroup (S a) where
  (S x) <> (S y) = S $ x + y

instance Num a => Monoid' (S a) where
  smoosh [] = S 50
  smoosh (x : xs) = x <> (smoosh xs)

instance Functor S where
  fmap f (S x) = S (f x)


data Nope a = NopeDotJpg deriving (Eq, Show)

instance Functor Nope where
  fmap f x = NopeDotJpg

instance Applicative Nope where
  pure x = NopeDotJpg
  x <*> y = NopeDotJpg

instance Monad Nope where
  x >>= f = NopeDotJpg

instance Arbitrary (Nope a) where
  arbitrary = return NopeDotJpg

instance EqProp (Nope a) where
  (=-=) = eq

data BahEither b a = PLeft a | PRight b deriving (Eq, Show)

instance Functor (BahEither b) where
  fmap _ (PRight b) = PRight b
  fmap f (PLeft a) = PLeft (f a)

instance Applicative (BahEither b) where
  pure x = PLeft x
  (PRight x) <*> _ = PRight x
  _ <*> (PRight x) = PRight x
  (PLeft f) <*> (PLeft x) = PLeft (f x)

instance Monoid b => Monad (BahEither b) where
  (PRight x) >>= _ = PRight x
  (PLeft x) >>= f = f x

instance (Arbitrary a, Arbitrary b) => Arbitrary (BahEither b a) where
  arbitrary = do
    a <- arbitrary 
    b <- arbitrary
    frequency [(1, return $ PLeft a), (1, return $ PRight b)]

instance (Eq a, Eq b) => EqProp (BahEither b a) where
  (=-=) = eq


newtype Identity a = Identity a deriving (Eq, Show, Ord)

instance Functor Identity where
  fmap f (Identity x) = Identity (f x)

instance Applicative Identity where
  pure x = Identity x
  (Identity f) <*> (Identity x) = Identity $ f x

instance (Arbitrary a) => Arbitrary (Identity a) where
  arbitrary = do
    a <- arbitrary
    return $ Identity a

instance Eq a => EqProp (Identity a) where
  (=-=) = eq

instance Monad Identity where
  return = pure
  (Identity a) >>= f = f a


instance Monad List where
  return = pure
  (Nil) >>= f = Nil
  (Cons x xs) >>= f = f x `append` (xs >>= f) 






testMonads :: IO ()
testMonads = do
  let nope :: Nope (Int, String, Char)
      nope = undefined
      bahEither :: BahEither String (Int, String, Char)
      bahEither = undefined
      identity :: Identity (Int, String, Char)
      identity = undefined
      list :: List (Int, String, Char)
      list = undefined
  quickBatch $ functor nope
  quickBatch $ applicative nope
  quickBatch $ monad nope
  quickBatch $ functor bahEither
  quickBatch $ applicative bahEither
  quickBatch $ monad bahEither
  quickBatch $ functor identity
  quickBatch $ applicative identity
  quickBatch $ monad identity
  quickBatch $ functor list
  quickBatch $ applicative list
  quickBatch $ monad list


j :: Monad m => m (m a) -> m a
j mma = mma >>= id

l1 :: Monad m => (a -> b) -> m a -> m b
l1 f ma = 
     fmap f ma

l1' :: Monad m => (a -> b) -> m a -> m b
l1' f ma = 
  ma >>= (return . f)

apply' :: Monad m => m (a -> b) -> m a -> m b
apply' mf ma = do 
  f <- mf
  a <- ma
  return $ f a

apply'' :: Monad m => m (a -> b) -> m a -> m b
apply'' mf ma =
  ma >>= 
    \a ->
      mf >>=
        \f ->
          return $ f a
        

l2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
l2 f ma mb =
  ma >>=
    \ a -> 
      mb >>= 
      \b ->
        return $ f a b

a :: Monad m => m a -> m (a -> b) -> m b
a ma mf =
  ma >>= 
    \a ->
      mf >>=
        \f ->
          return $ f a

meh :: Monad m => [a] -> (a -> m b) -> m [b]
meh [] f = pure []
meh (a : as) f = l2 (:) (f a)  (meh as f)

expandMeh :: Monad m => [a] -> (a -> m b) -> m [b]
expandMeh [] f = pure []
expandMeh (a : as) f = (f a) >>=
                          \b -> 
                            (meh as f) >>=
                              \bs -> 
                                return $ b : bs

flipType :: (Monad m) => [m a] -> m [a]
flipType ms = meh ms id


bullshit =
  \x -> 
    \a -> 
      x + a
