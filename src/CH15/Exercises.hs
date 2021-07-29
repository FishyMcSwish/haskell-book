module CH15.Exercises where

import Data.Semigroup
import Test.QuickCheck


data Optional a  = 
  Nada
  | Only a
  deriving (Eq, Show)


instance Semigroup a => Semigroup (Optional a) where
  (<>) (Only a1) (Only a2) = Only (a1 <> a2)
  (<>) (Only a1) _ = Only a1
  (<>) _ (Only a2) = Only a2
  (<>) Nada Nada = Nada

instance Monoid a => Monoid (Optional a) where
  mempty = Nada
  

type Adjective = String
type Adverb = String
type Noun = String
type Exclamation = String

madlibbinBetter' :: Exclamation -> Adverb -> Noun -> Adjective -> String
madlibbinBetter' ex adv noun adj =
  mconcat [ex, "! he said ", adv, " as he juimped into his car ", noun, " and drove off with his ", adj, " wife"]



-- writing my own instance of First, a Monoid type class for Maybe, except
-- mine won't require the contents to be monoidal.  I will have to just pick one 
-- on mappend.
-- then testing the monoid laws with quickcheck.
newtype First' a = First' {getFirst' :: Optional a} deriving (Eq, Show)

instance Semigroup (First' a) where
  (<>) left@(First' (Only a))  _ = left
  (<>) _ right@(First' (Only a)) = right 
  (<>) _ _ = First' Nada

instance Monoid (First' a) where
  mempty = First' {getFirst' = Nada}

instance (Arbitrary a) => Arbitrary (First' a) where
  arbitrary = do
    a <- arbitrary
    frequency [(5, return $ First' (Only a)), (1, return $ First' Nada)]

firstMappend :: First' a -> First' a -> First' a
firstMappend = mappend

type FirstMappend = First' String -> First' String -> First' String  -> Bool

type FstId = First' String -> Bool


---validate with quick check here

monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity a = (mempty <> a ) == a

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity a = (a <> mempty) == a

monoidAssoc :: (Eq m, Monoid m) => m -> m -> m -> Bool
monoidAssoc a b c=
  a <> (b <> c) == (a <> b) <> c

combineLeftIdentity :: (Eq b, Monoid b) => Combine a b -> a -> Bool
combineLeftIdentity f a =
  (unCombine $ f <> mempty) a == (unCombine f)  a

combineRightIdentity :: (Eq b, Monoid b) => Combine a b -> a -> Bool
combineRightIdentity f a =
  (unCombine $ mempty <> f) a == (unCombine f)  a

compLeftIdentity :: (Eq a) => Comp a -> a -> Bool
compLeftIdentity f a =
  (unComp $ f <> mempty) a == (unComp f)  a

compRightIdentity :: (Eq a) => Comp a -> a -> Bool
compRightIdentity f a =
  (unComp $ mempty <> f) a == (unComp f)  a

doIt :: IO ()
doIt = do 
  quickCheck (monoidAssoc:: FirstMappend)
  quickCheck (monoidLeftIdentity :: FstId)
  quickCheck (monoidRightIdentity :: FstId)



--some fns to test iwth

onlyOne = First' (Only 1)
onlyTwo = First' (Only 2)
nada = First' Nada


--Semigroup exercises
data Trivial = Trivial deriving (Eq, Show)

instance Semigroup Trivial where
  Trivial <> Trivial = Trivial

instance Arbitrary Trivial where
  arbitrary = return Trivial
  
type TrivAssoc = Trivial -> Trivial -> Trivial -> Bool 


newtype Identity a = Identity a deriving (Eq, Show)

instance (Semigroup a) => Semigroup (Identity a) where
  (Identity x) <> (Identity y) = Identity (x <> y)

instance (Arbitrary a) => Arbitrary (Identity a) where
  arbitrary = do
    a <- arbitrary
    return $ Identity a


data Two a b = Two a b deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
  (Two a b) <> (Two a' b') = Two (a <> a') (b <> b')

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return $ Two a b

data Three a b c = Three a b c deriving (Eq, Show)

instance (Semigroup a, Semigroup b, Semigroup c) => Semigroup (Three a b c) where
  (Three a b c) <> (Three a' b' c') = Three (a <> a') (b <> b') (c <> c')

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = do
    a <- arbitrary 
    b <- arbitrary 
    c <- arbitrary 
    return $ Three a b c

data Four a b c d = Four a b c d deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Arbitrary (Four a b c d) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    d <- arbitrary
    return $ Four a b c d 

instance (Semigroup a, Semigroup b, Semigroup c, Semigroup d) => Semigroup (Four a b c d) where
  (Four a b c d) <> (Four a' b' c' d') = 
    Four (a <> a') (b <> b') (c <> c') (d <> d') 



newtype BoolConj = BoolConj Bool deriving (Eq, Show)

instance Semigroup BoolConj where
  (BoolConj True) <> (BoolConj True) = BoolConj True
  _ <> _ = BoolConj False

instance Arbitrary BoolConj where
  arbitrary = do
    a <- arbitrary
    return $ BoolConj a


newtype BoolDisj = BoolDisj Bool deriving (Eq, Show)

instance Semigroup BoolDisj where
  (BoolDisj False) <> (BoolDisj False) = BoolDisj False
  _ <> _ = BoolDisj True

instance Arbitrary BoolDisj where
  arbitrary = do
    a <- arbitrary 
    return $ BoolDisj a


data Or a b = Fst a | Snd b deriving (Eq, Show)

--it holds the first Snd it gets, otherwise the most recent Fst
instance Semigroup (Or a b) where
  (Snd b) <> _ =  Snd b
  _ <> (Snd b') = Snd b'
  (Fst a) <> (Fst a') = Fst a'


instance (Arbitrary a, Arbitrary b) => Arbitrary (Or a b) where
  arbitrary = do 
    a <- arbitrary
    b <- arbitrary
    elements [Fst a, Snd b]


newtype Combine a b = Combine {unCombine :: (a -> b)}

instance Show (Combine a b) where
  show x = "It's a Combine!"

instance (CoArbitrary a, Arbitrary b) => Arbitrary (Combine a b) where
  arbitrary = do
    f <- arbitrary :: (CoArbitrary a, Arbitrary b) => Gen (a -> b)
    return $ Combine f

instance (Semigroup s) => Semigroup (Combine a s) where
  (Combine f1) <> (Combine f2) = Combine (\x -> (f1 x) <> (f2 x))  


newtype Combine' a b = Combine' {unCombine' :: (a -> b)} 

--why does this work?  why don't i need to use some operator for this or
--something? e.g. i can do (show <> show) 1
instance (Semigroup s) => Semigroup (Combine' a s) where
  (Combine' f1) <> (Combine' f2) = Combine' (f1 <> f2)



newtype Comp a = Comp {unComp :: (a -> a)}

instance Show (Comp a) where
  show x = "It's a comp"

instance Semigroup (Comp a) where
  Comp f <> Comp g = Comp (f . g)

instance Monoid (Comp a) where
  mempty = Comp id

instance (CoArbitrary a, Arbitrary a) => Arbitrary (Comp a) where
  arbitrary = do
    f <- arbitrary :: (CoArbitrary a, Arbitrary a) => Gen (a -> a)
    return $ Comp f
    

data Validation' a b = Failure' a | Success' b deriving (Eq, Show)

--keeps the first success, munges failures
instance Semigroup a => Semigroup (Validation' a b) where
  (Failure' a) <> (Failure' b) = Failure' (a <> b)
  (Success' a) <> _ = Success' a
  _ <> Success' b = Success' b

fail :: String -> Validation' String Int
fail = Failure'

success :: Int -> Validation' String Int
success = Success'


--Monoid exercises

instance Monoid Trivial where
  mempty = Trivial

instance (Monoid a) => Monoid (Identity a) where
  mempty = Identity mempty 

instance (Monoid a, Monoid b) => Monoid (Two a b) where
  mempty = Two mempty mempty

instance Monoid BoolConj where
  mempty = BoolConj True

instance Monoid BoolDisj where
  mempty = BoolDisj False

instance (Monoid b) => Monoid (Combine a b) where
  mempty = Combine mempty
  
newtype Mem s a = Mem { runMem :: s -> (a, s)}

instance Semigroup a => Semigroup (Mem s a) where
  (Mem f1) <> (Mem f2) = Mem (\s -> ((fst $ f1 s) <> (fst $ f2 s), snd .f1 . snd . f2 $ s))

instance Monoid a => Monoid (Mem s a) where
  mempty = Mem (\s -> (mempty, s))

memFn = Mem $ \s -> ("hi", s + 1)
memFn' = Mem $ \s -> ("hi", s - 1)
memFn'' = Mem $ \s -> ("hi", s - 2)
memFn''' = Mem $ \s -> ("hi", s - 3)

subtractionMemFn1 = runMem (memFn' <> (memFn'' <> memFn''')) 0
subtractionMemFn2 = runMem ((memFn' <> memFn'') <> memFn''') 0
subFn1 =  ( (-1) - ((-2) - 3))
subFn2 =  ( ((- 1) - 2) - 3)
-- because i'm not parenthesizing, affecting order of operations, not pumping
-- results into the next fn invcrementally every time, a la composition.

testMem :: IO ()
testMem = do
  let rmzero = runMem mempty 0
      rmleft = runMem (memFn <> mempty) 0
      rmright = runMem (mempty <> memFn) 0
  print $ rmleft
  print $ rmright
  print $ (rmzero :: (String, Int))
  print $ rmleft == runMem memFn 0
  print $ rmright == runMem memFn 0


semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc x y z =
  x <> (y <> z) == (x <> y) <> z

combineAssoc :: (Eq b, Semigroup b) => Combine a b ->  Combine a b ->  Combine a b -> a -> Bool
combineAssoc f g h a =
  (unCombine (f <> (g <> h))) a == (unCombine ((f <> g) <> h)) a

compAssoc :: (Eq a) => Comp a -> Comp a -> Comp a -> a -> Bool
compAssoc f g h a =
  (unComp (f <> (g <> h))) a == (unComp ((f <> g) <> h)) a


 
testExercises :: IO ()
testExercises = do
  quickCheck (semigroupAssoc :: TrivAssoc)
  quickCheck (semigroupAssoc :: Identity String -> Identity String -> Identity String -> Bool)
  quickCheck (semigroupAssoc :: Two String [Int] -> Two String [Int] -> Two String [Int] -> Bool)
  quickCheck (semigroupAssoc :: Three String [Int] (Sum Int) -> Three String [Int] (Sum Int) -> Three String [Int] (Sum Int) -> Bool)
  quickCheck (semigroupAssoc :: Four String [Int] (Sum Int) (Product Rational) -> Four String [Int] (Sum Int) (Product Rational) -> Four String [Int] (Sum Int) (Product Rational) -> Bool)

  quickCheck (semigroupAssoc :: BoolConj -> BoolConj -> BoolConj -> Bool)
  quickCheck (semigroupAssoc :: BoolDisj -> BoolDisj -> BoolDisj -> Bool)
  quickCheck (semigroupAssoc :: Or String [Int] -> Or String [Int] -> Or String [Int] -> Bool)
  quickCheck (combineAssoc :: Combine Int String -> Combine Int String -> Combine Int String -> Int -> Bool)
  quickCheck (compAssoc :: Comp Int-> Comp Int-> Comp Int-> Int -> Bool)
  quickCheck (monoidLeftIdentity :: Trivial -> Bool)
  quickCheck (monoidRightIdentity :: Trivial -> Bool)
  quickCheck (monoidLeftIdentity :: Identity String -> Bool)
  quickCheck (monoidRightIdentity :: Identity String -> Bool)
  quickCheck (monoidLeftIdentity :: Two String [Int] -> Bool)
  quickCheck (monoidRightIdentity :: Two String [Int] -> Bool)
  quickCheck (monoidLeftIdentity :: BoolConj -> Bool)
  quickCheck (monoidRightIdentity :: BoolConj -> Bool)
  quickCheck (monoidLeftIdentity :: BoolDisj -> Bool)
  quickCheck (monoidRightIdentity :: BoolDisj -> Bool)
  quickCheck (combineLeftIdentity :: Combine Int String -> Int -> Bool)
  quickCheck (combineRightIdentity :: Combine Int String -> Int -> Bool)
  quickCheck (compLeftIdentity :: Comp Char-> Char -> Bool)
  quickCheck (compRightIdentity :: Comp Char-> Char -> Bool)



f = Combine $ \n -> Sum (n + 1)
g = Combine $ \n -> Sum (n - 1)
f' = Combine' $ \n -> Sum (n + 1)
g' = Combine' $ \n -> Sum (n - 1)
