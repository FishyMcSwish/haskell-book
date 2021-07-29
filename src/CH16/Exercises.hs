{-#LANGUAGE FlexibleInstances#-}

module CH16.Exercises where

import Test.QuickCheck
import GHC.Arr

replaceWithP :: b -> Char
replaceWithP = const 'p'


lmls :: [Maybe ( [String] )]
lmls = [Just ["Ha", "ha"], Nothing, Just []]

lms :: [Maybe String]
lms = [Just "Ave", Nothing, Just "woohooo"]

replaceWithP' :: [Maybe [Char]] -> Char
replaceWithP'= replaceWithP

liftedReplace :: Functor f => f a -> f Char
liftedReplace = fmap replaceWithP

liftedReplace' :: [Maybe [Char]] -> [Char]
liftedReplace' = liftedReplace

twiceLifted :: (Functor f1, Functor f) => f (f1 a) -> f (f1 Char)
twiceLifted = (fmap . fmap) replaceWithP

twiceLifted' :: [Maybe [Char]] -> [Maybe Char]
twiceLifted' = twiceLifted

thriceLifted :: (Functor f2, Functor f1, Functor f) =>  f ( f1 (f2 a)) -> f ( f1 ( f2 Char))
thriceLifted = (fmap . fmap . fmap) replaceWithP

thriceLifted' :: [Maybe [Char]] -> [Maybe [Char]]
thriceLifted' = thriceLifted

printFmaps :: IO ()
printFmaps = do
  putStr "lms : " 
  print lms
  putStr "replaceWithP' lms : " 
  print (replaceWithP' lms)
  putStr "liftedReplace lms" 
  print (liftedReplace lms)
  putStr "liftedReplace' lms: " 
  print $ liftedReplace' lms
  putStr "twiceLifted lms: "
  print ( twiceLifted lms )
  putStr "twiceLfited' lms : " 
  print ( twiceLifted' lms )
  putStr "thriceLifted lms : " 
  print ( thriceLifted lms )
  putStr "thriceLifted' lms : " 
  print ( thriceLifted' lms )


-- heavy lifting exercise
--
a = fmap (+1) $ read "[1]" :: [Int]
b = (fmap . fmap) (++ "lol") (Just ["Hi,", "Hello"])
c = (*2) . (\x -> x - 2)
d = ((return '1' ++) . show) . (\x -> [x, 1..3])
e :: IO Integer
e = let ioi = readIO "1" :: IO Integer
        changed = fmap (read . ("123"++) . show) ioi
    in fmap (*3) changed
    
--Quick checking functors

functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
functorIdentity f = 
  fmap id f == f

functorCompose :: (Functor f, Eq (f c)) => (a -> b) -> (b -> c) -> f a -> Bool
functorCompose f g x =
  (fmap (g . f) x) == (fmap g ( fmap f x))

newtype Identity a = Identity a deriving (Eq, Show)

instance Functor Identity where
  fmap f (Identity x) = Identity (f x)

instance (Arbitrary a) => Arbitrary (Identity a) where
  arbitrary = do
    a <- arbitrary
    return $ Identity a

data Pair a = Pair a a deriving (Eq, Show)

instance Functor Pair where
  fmap f (Pair x y ) = Pair (f x) (f y)

instance (Arbitrary a) => Arbitrary (Pair a) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return $ Pair a b

data Two a b = Two a b deriving (Eq, Show)

instance Functor (Two a) where
  fmap f (Two x y) = Two x (f y)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return $ Two a b

data Three a b c = Three a b c deriving (Eq, Show)

instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b (f c)

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = do 
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return $ Three a b c


data Three' a b = Three' a b b deriving (Eq, Show)

instance Functor (Three' a)  where
  fmap f (Three' x y z) = Three' x (f y) (f z)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return $ Three' a b c
  
data Four a b c d = Four a b c d deriving (Eq, Show)

instance Functor (Four a b c) where
  fmap f (Four x y z n) = Four x y z (f n)

instance (Arbitrary a,Arbitrary b,Arbitrary c,Arbitrary d) => Arbitrary (Four a b c d) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    d <- arbitrary
    return $ Four a b c d


data Four' a b = Four' a a a b deriving (Eq, Show)

instance Functor (Four' a) where
  fmap f (Four' a b c d) = Four' a b c (f d)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    d <- arbitrary
    return $ Four' a b c d

data Possibly a = Yep a | Nope deriving (Eq, Show)

instance Functor Possibly where
  fmap _ Nope = Nope
  fmap f (Yep x) = Yep (f x)

data Sum a b = First a | Second b deriving (Eq, Show)

instance Functor (Sum a) where
  fmap f (First x) = First x
  fmap f (Second b) = Second (f b)

testConst :: IO ()
testConst = do
  putStr "fmap ca c1"
  print $ fmap ca c1
  putStr "fmap cb c1"
  print $ fmap cb c1
  putStr "cb c1"
  print $ cb c1
  putStr "fmap (ca . cb) c1"
  print $ fmap (ca . cb) c1
  putStr "fmap ca (fmap cb c1)"
  print $ fmap ca (fmap cb c1)


testFunctorLaws :: IO ()
testFunctorLaws = do
  quickCheck (functorIdentity :: [Int] -> Bool)
  quickCheck (functorCompose (+1) (*2) :: [Int] -> Bool)
  quickCheck (functorIdentity :: Identity Int -> Bool)
  quickCheck (functorCompose (+1) (*2) :: Identity Int -> Bool)
  quickCheck (functorIdentity :: (Pair Int) -> Bool)
  quickCheck (functorCompose (+1) (*2) :: (Pair Int) -> Bool)
  quickCheck (functorIdentity :: (Two String Int) -> Bool)
  quickCheck (functorCompose (+1) (*2) :: (Two String Int) -> Bool)
  quickCheck (functorIdentity :: (Three Bool String Int) -> Bool)
  quickCheck (functorCompose (+1) (*2) :: (Three Bool String Int) -> Bool)
  quickCheck (functorIdentity :: (Three' String Int) -> Bool)
  quickCheck (functorCompose (+1) (*2) :: (Three' String Int) -> Bool)
  quickCheck (functorIdentity :: (Four Bool Char String Int) -> Bool)
  quickCheck (functorCompose (+1) (*2) :: (Four Char Bool String Int) -> Bool)
  quickCheck (functorIdentity :: (Four' Bool Int) -> Bool)
  quickCheck (functorCompose (+1) (*2) :: (Four' Bool Int) -> Bool)

-- messing with Constant functor
newtype Constant a b = 
  Constant {getConstant :: a} deriving (Eq, Show)

instance Functor (Constant a) where
  fmap f (Constant x) = Constant x

c1 :: Constant Int b
c1 = Constant 1

c2 :: Constant Int b
c2 = Constant 2

ca = const "a"
cb = const "b"


--Chapter exercises
--functor can't be written for Bool, it is kind *

data BoolAndSomethingElse a = False' a | True' a

instance Functor BoolAndSomethingElse where
  fmap f (True' x) = True' (f x)
  fmap f (False' x) = False' (f x)

data BoolAndMaybeSomethingElse a = Falsish | Truish a

instance Functor BoolAndMaybeSomethingElse where
  fmap _ Falsish = Falsish
  fmap f (Truish a) = Truish (f a)

newtype Mu f = InF {outF :: f (Mu f)} 
--can't be written because f has kind (* -> *). so Mu is (* -> *) -> *.
--if you try instance Functor (Mu a)..., then you have kind *.
  --
data D = D (Array Word Word) Int Int
--no functor instance possible because it has kind *


--Re arranging arguments to type constructgoss to fix Functor instances

data Sum' b a = First' a | Second' b

instance Functor (Sum' e) where
  fmap f (First' a )  = First' (f a)
  fmap f (Second' b) = Second' b
  

data Company a b c = 
  DeepBlue a b
    | Something c

instance Functor (Company e e') where
  fmap f (Something b) = Something (f b)
  fmap _ (DeepBlue a c) = DeepBlue a c

data More b a =
  L a b a | R  b a b deriving (Eq, Show)

instance Functor (More x) where
  fmap f (L a b a') = L (f a) b (f a')
  fmap f (R b a b') = R b (f a) b'

data Quant a b =
  Finance 
    | Desk a 
    | Bloor b

instance Functor (Quant a) where
  fmap _ Finance = Finance
  fmap _ (Desk x) = Desk x
  fmap f (Bloor x) = Bloor (f x)


data K a b = K a deriving (Eq, Show)

instance Functor (K a) where
  fmap _ (K x) = K x

newtype Flip f a b = Flip (f b a) deriving (Eq, Show)

newtype K' a b = K' a

instance  Functor (Flip K a) where
  fmap f (Flip (K x)) = Flip (K (f x))

instance Functor (Flip K' a) where
  fmap f (Flip (K' x)) = Flip (K' (f x))

data  EvilGoateeConst a b =
  GoatyConst b

instance Functor (EvilGoateeConst a) where
  fmap f (GoatyConst b) = GoatyConst (f b)

data LiftItOut f a = 
  LiftItOut (f a) deriving (Eq, Show)

instance (Functor f) => Functor (LiftItOut f) where
  fmap f2 (LiftItOut fa) = LiftItOut (fmap f2 fa)


data Parappa f g a =
  DaWrappa (f a) (g a)

instance (Functor f, Functor g) => Functor (Parappa f g) where
  fmap f (DaWrappa fa ga) = DaWrappa (fmap f fa) (fmap f ga)


data IgnoreOne f g a b =
  IngoringSomething (f a) (g b)

instance (Functor g) => Functor (IgnoreOne f g a) where
  fmap f (IngoringSomething fa gb) = IngoringSomething fa (fmap f gb)


data List a = Nil | Cons a (List a)

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons x (xs)) = Cons (f x) (fmap f xs)

data GoatLord a = 
  NoGoat
    | OneGoat a
    | MoreGoats (GoatLord a)
      (GoatLord a)
      (GoatLord a)
      deriving (Eq, Show)

instance Functor GoatLord where
  fmap _ NoGoat = NoGoat
  fmap f (OneGoat x) = OneGoat (f x)
  fmap f (MoreGoats l m r) = MoreGoats (fmap f l)(fmap f m)(fmap f r)
  
data TalkToMe a = 
  Halt
    | Print String a
    | Read (String  -> a)

instance (Show a) => Show (TalkToMe a) where
  show Halt = "halt"
  show (Print x y) = "Print " ++ show x ++ show y
  show (Read fn) = "Read"

instance Functor TalkToMe where
  fmap _ Halt = Halt
  fmap f (Print str x) = Print str (f x)
  fmap f (Read fn) = Read (f . fn)


doReading :: TalkToMe a -> String -> Maybe a
doReading (Read fn) str = Just $ fn str
doReading _ _ = Nothing
