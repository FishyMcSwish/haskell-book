{-# LANGUAGE FlexibleInstances #-}
module CH11.Exercises where

import Data.Char
import Data.List
  
data Doggies a =
    Husky a
  | Mastiff a
  deriving (Eq, Show)

data DogueDeBordeaux doge = DogueDeBordeaux doge

data Price =
  Price Integer deriving (Eq, Show)

data Manufacturer =
  Mini
    | Mazda
    | Tata
    deriving (Eq, Show)

data Size =
  Size Integer deriving (Eq, Show)

data Airline =
  PapuAir
    | CatapultsR'Us
    | TakeYourChancesUnited
    deriving (Eq, Show)
     
data Vehicle =
  Car Manufacturer Price
    | Plane Airline Size
    deriving (Eq, Show)

myCar = Car Mini (Price 140)
urCar = Car Mazda (Price 200)
clownCar = Car Tata (Price 700)
doge = Plane PapuAir (Size 1)

isCar :: Vehicle -> Bool
isCar (Car _ _) = True
isCar _ = False

isPlane :: Vehicle -> Bool
isPlane ( Plane _ _ )= True
isPlane _ = False

areCars :: [Vehicle] -> [Bool]
areCars = map isCar


data Example = MyExample deriving Show
data Example' = MyExample' Int deriving Show


class TooMany a where 
  tooMany :: a -> Bool

instance TooMany Int where 
  tooMany n = n > 42

newtype Goats = Goats Int deriving Show

instance TooMany Goats where
  tooMany (Goats n) = n > 43

type Goats' = Int

-- not allowed!
-- instance TooMany Goats' where
--  tooMany (Goats n) = n > 43

newtype Whatever = Whatever (Int, String) deriving Show

instance TooMany Whatever where
  tooMany (Whatever (x, _)) = tooMany x

newtype MultiGoats = MultiGoats (Int, Int) deriving Show

instance TooMany MultiGoats where
  tooMany (MultiGoats (x, y)) = tooMany x || tooMany y

instance (Num a, TooMany a) => TooMany (a, a) where
  tooMany (x, y) = False


--"sum of products" or normal form
data FlowerType = Gardenia | Daisy | Rose | Lilac deriving Show
type Gardener = String
data Garden = Garden Gardener FlowerType deriving Show

data NormalFormGarden = 
  NFGardenia Gardener
    | NFDaisy Gardener
    | NFRose Gardener
    | NFLilac Gardener


-- Programmers
--

data OperatingSystem = 
  GnuPlusLinux
    | OpenBSDPlusNevermindJustBSDStill
    | Mac
    | Windows
    deriving (Eq, Show)

data ProgLang =
  Haskell 
    | Agda
    | Idris 
    | PureScript
    deriving (Eq, Show)

data Programmer = 
  Programmer { os :: OperatingSystem
             , lang :: ProgLang}
             deriving (Eq, Show)


allOperatingSystems :: [OperatingSystem]
allOperatingSystems = 
  [ GnuPlusLinux
    , OpenBSDPlusNevermindJustBSDStill
    , Mac
    , Windows
  ]

allLanguages :: [ProgLang]
allLanguages =
  [ Haskell 
    , Agda
    , Idris 
    , PureScript
  ]

allProgrammers :: [Programmer]
allProgrammers =
  [ Programmer {os = x, lang = y} | x <- allOperatingSystems, y <- allLanguages]


--What is the cardinality, or number of possible forms, of these expressions?

data Quad = One
          | Two
          | Three
          | Four
          deriving (Eq, Show)

--Either is a sum, 4 + 4 = 8
aQuad :: Either Quad Quad
aQuad = undefined

--Tuple is a product type, 4 * 4 = 16
prodQuad :: (Quad, Quad)
prodQuad = undefined

-- 4^4 = 256
funcQuad :: Quad -> Quad
funcQuad = undefined

-- 2 * 2 * 2 = 8
prodTBool :: (Bool, Bool, Bool)
prodTBool = undefined

--2 ^ (2*2) = 16
gTwo :: Bool -> Bool -> Bool
gTwo = undefined

--4 ^ (4*2) = 4 ^ 8 = 65536
fTwo :: Bool -> Quad -> Quad
fTwo = undefined

--as-pattern exercises

isSubseqOf :: (Eq a) => [a] ->[a] -> Bool
isSubseqOf [] _ = True
isSubseqOf _ [] = False
isSubseqOf allAs@(a:restAs) (b:restBs) =
  if a == b then 
    isSubseqOf restAs restBs
  else
    isSubseqOf allAs restBs

capitalizeWords :: String -> [(String, String)]
capitalizeWords str =
  map tupleWithCap . words $ str

tupleWithCap :: String -> (String, String)
tupleWithCap [] = ("","")
tupleWithCap str@(a:as) = (str, (toUpper a):as) 

-- language exercises

capitalizeWord :: String -> String
capitalizeWord [] = ""
capitalizeWord (a:as) = (toUpper a) : as

capitalizeParagraph :: String -> String
capitalizeParagraph = unwords . doCaps . words
  where
    doCaps [] = []
    doCaps all@(first:_) = capitalizeWord first : capitalizeAfterPeriod all
    endOfSentence [] = False
    endOfSentence w = elem (last w) ".?!"
    capitalizeAfterPeriod (prev:this:rest) = 
      if endOfSentence prev then
        capitalizeWord this : capitalizeAfterPeriod (this: rest)
      else
        this : capitalizeAfterPeriod (this : rest)
    capitalizeAfterPeriod _ = []


-- phone exercise

data DaPhone = DaPhone [Button] deriving Show
data Button = Button Digit [Char] deriving Show
type Digit = Char
type Presses = Int

ourPhone = DaPhone [Button '1' "1"
                   , Button '2' "abc2"
                   , Button '3' "def3"
                   , Button '4' "ghi4"
                   , Button '5' "jkl5"
                   , Button '6' "mno6"
                   , Button '7' "pqrs7"
                   , Button '8' "tuv8"
                   , Button '9'"wxyz9"
                   , Button '*' "^*"
                   , Button '0' " +0"
                   , Button '#' ".,#"]

pressesForChar ::  Button -> Char -> Presses
pressesForChar  (Button _ btnChars) char = 
  case elemIndex char btnChars of
    Just index ->
      index + 1
    Nothing ->
      error "we messed up"

reverseTaps :: DaPhone -> Char -> [(Digit, Presses)]
reverseTaps (DaPhone buttons) char =
  if isUpper char then
   ('*', 1) : [(digit, presses)]
  else
    [(digit, presses)]
      where 
        charToFind = toLower char
        button@(Button digit _) =  head $ filter (\(Button _ chars) -> elem charToFind $ chars)  buttons
        presses =  pressesForChar button charToFind

pressesForMessage :: DaPhone -> String -> [(Digit, Presses)]
pressesForMessage phone str =
  concatMap (reverseTaps phone) str

fingerTaps :: [(Digit, Presses)] -> Presses
fingerTaps =
  foldr (\(_, presses) acc -> acc + presses) 0

mostPopularLetter :: String -> Char
mostPopularLetter =
  mostCommonElement . filter isAlpha

coolestLtr :: [String] -> Char
coolestLtr = 
  mostPopularLetter . foldr (++) []   

coolestWord :: [String] -> String
coolestWord =
  mostCommonElement . words . concatMap (\x -> x ++ [' '])

mostCommonElement :: (Ord a, Eq a) => [a] -> a
mostCommonElement =
  head . maximumBy (\x y -> compare  (length x) (length y)) . group . sort 


--Hutton's razor

data Expr = Lit Integer | Add Expr Expr

eval :: Expr -> Integer
eval (Lit x) = x
eval (Add expr1 expr2) = (eval expr1) + (eval expr2) 

printExpr :: Expr -> String
printExpr (Lit x) = show x
printExpr (Add expr1 expr2) = (printExpr expr1) ++ " + " ++ (printExpr expr2)


