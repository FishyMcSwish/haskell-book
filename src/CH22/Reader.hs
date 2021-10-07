{-# LANGUAGE InstanceSigs #-}
module CH22.Reader where

import Control.Applicative
import Data.Char

boop = (*2)
doop = (+10)

bip :: Integer -> Integer 
bip = boop . doop

bloop = fmap boop doop


-- functor, applicative, monad of functions
bbop :: Integer -> Integer
bbop = (+) <$> boop <*> doop

duwop :: Integer -> Integer
duwop = liftA2 (+) boop doop

boopDoop :: Integer -> Integer 
boopDoop = do
  a <- boop
  b <- doop
  return $ a + b


cap :: [Char] -> [Char]
cap xs = map toUpper xs

rev :: [Char] -> [Char] 
rev xs = reverse xs

composed :: [Char] -> [Char]
composed = cap . rev

fmapped :: [Char] -> [Char]
fmapped = fmap cap rev

tupled :: [Char] -> ([Char], [Char])
tupled = 
  (,) <$> cap <*> rev

tupledM :: [Char] -> ([Char], [Char])
tupledM = do
  a <- cap
  b <- rev
  return (a, b)

--reader is just a function, uses the ((->) r) instance for monad, applicative,
--functor.
newtype Reader r a = Reader {unReader :: r -> a}

ask :: Reader a a
ask = Reader id


-- example of using reader / function applications applicative instance

newtype HumanName = HumanName String deriving (Eq, Show)
newtype DogName = DogName String deriving (Eq, Show)
newtype Address = Address String deriving (Eq, Show)

data Person = Person
  { humanName :: HumanName
  , dogName :: DogName
  , address :: Address
  }
  deriving (Eq, Show)
data Dog = Dog{
                     dogsName :: DogName
                    , dogsAddress :: Address
                    }
                    deriving (Eq, Show)


pers = Person (HumanName "guy") (DogName "dog") (Address "addy")

--not using applicative of function / reader
getDog :: Person -> Dog
getDog p =
  Dog (dogName p) (address p)

-- using it
getDogR :: Person -> Dog
getDogR =
  Dog <$> dogName <*> address

getDogM :: Person -> Dog
getDogM = do
  name <- dogName
  addy <- address
  return $ Dog name addy


-- r in ((->) r) is Person
-- ((->) r) is just a Monad m
-- Person -> ??? == m
-- 
getDogM'' :: Person -> Dog
getDogM'' =
  (dogName >>= 
    \name ->
      address >>= 
        \addy ->
          return $ Dog name addy) 

getDogM' :: Reader Person Dog
getDogM' = do
  name <- Reader dogName
  addy <- Reader  address
  return $ Dog name addy

asks :: (r -> a) -> Reader r a
asks = Reader 

instance Functor (Reader r) where
  fmap f (Reader ra) = Reader $ (f . ra)

instance Applicative (Reader r) where
  pure :: a -> Reader r a
  pure a = Reader $ const a

  (<*>) :: Reader r (a -> b) -> Reader r a -> Reader r b
  (Reader rab) <*> (Reader ra) = Reader $ \r -> (rab r (ra r))


instance Monad (Reader r) where
  return = pure
  (>>=) :: Reader r a -> (a -> Reader r b) -> Reader r b
  (Reader ra) >>= aRb = Reader $ \r -> unReader (aRb (ra r)) r

myLiftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
myLiftA2 g fa fb =
  g <$> fa <*> fb
