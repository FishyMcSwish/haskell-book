module CH12.Exercises where

import CH11.BinaryTree

notThe :: String -> Maybe String
notThe "the" = Nothing
notThe str = Just str

replaceThe :: String -> String
replaceThe = 
  unwords. map (maybe "a" id) . map notThe . words

--oops it was supposed to be recursive
replaceTheR :: String -> String
replaceTheR str = unwords $ go (words str) where
  go [] = []
  go (x:xs) = maybe "a" id (notThe x) : (go xs)

startsWithVowel :: String -> Bool
startsWithVowel [] = False
startsWithVowel (a:as) = elem a "aeiou"

countTheBeforeVowel :: String -> Integer
countTheBeforeVowel str = go 0 (words str) where
  go acc (x1:x2:xs) = 
    if startsWithVowel x2 && x1 == "the" then
      go (acc + 1) (x2:xs)
    else
      go acc (x2:xs)
  go acc _  = acc


countVowels :: String -> Int
countVowels str = length $ onlyVowels str where
  onlyVowels [] = []
  onlyVowels (x : xs) = if elem x "aeiou" then
                          x : (onlyVowels xs)
                        else
                          onlyVowels xs

-- validate the word

newtype Word' = Word' String deriving (Eq, Show)

vowels = "aeiou"

mkWord :: String -> Maybe Word'
mkWord str
  | (length str) - 2 * (countVowels str) > 0 = Just $ Word' str
  | otherwise = Nothing

--It's only natural

data Nat = Zero | Succ (Nat) deriving (Eq, Show)

natToInteger :: Nat -> Integer 
natToInteger Zero = 0
natToInteger (Succ x) = 1 + natToInteger x

integeralToNat :: Integer -> Maybe Nat
integeralToNat x
  | x == 0 = Just Zero
  | x > 0 = fmap Succ $ integeralToNat (x-1)
  | otherwise = Nothing


-- small library for maybe
--
isJust :: Maybe a -> Bool
isJust (Just _) = True
isJust Nothing = False

isNothing :: Maybe a -> Bool
isNothing = not . isJust

mayybee :: b -> (a -> b) -> Maybe a -> b
mayybee dflt _ Nothing = dflt
mayybee _ fn (Just a) = fn a

fromMaybe :: a -> Maybe a -> a
fromMaybe dflt mybe = mayybee dflt id mybe 

listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe (a:_) = Just a

maybeToList :: Maybe a -> [a]
maybeToList Nothing = []
maybeToList (Just a) = [a]

catMaybes :: [Maybe a] -> [a]
catMaybes [] = []
catMaybes (x : xs) = case x of
                      Just y ->
                        y : catMaybes xs
                      Nothing ->
                        catMaybes xs

flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe =
  foldr smushMaybeList (Just[]) 

smushMaybeList :: Maybe a -> Maybe [a] -> Maybe [a]
smushMaybeList Nothing _ = Nothing
smushMaybeList _ Nothing = Nothing
smushMaybeList (Just a) (Just lst) = Just (a : lst)

-- small lib for either

lefts' :: [Either a b] -> [a]
lefts' =
  foldr smushLefts []

smushLefts :: Either a b -> [a] -> [a]
smushLefts (Right b) as = as
smushLefts (Left a) as = a : as

rights' :: [Either a b] -> [b]
rights' =
  foldr smushRights []
    where --smushRights :: Either a b -> [b] -> [b]
          smushRights ethr bs =
              case ethr of
                Left _ -> bs
                Right b -> b : bs

--i like the foldr better here
partitionEithers' :: [Either a b] -> ([a], [b])
partitionEithers' =
  foldr f ([],[])
    where 
      f (Left a) (as, bs) = (a:as, bs)
      f (Right b) (as, bs) = (as, b:bs)

--is it cleaner with a case with just one destructure?
partitionEithersCase' :: [Either a b] -> ([a], [b])
partitionEithersCase' =
  foldr f ([],[])
    where 
      f ethr (as, bs) =
        case ethr of
          Left a -> (a : as, bs)
          Right b -> (as, b : bs)

partitionEithers'' :: [Either a b] -> ([a], [b])
partitionEithers'' lst = 
  go lst ([],[])
    where
      go [] (as, bs) = (as,bs)
      go ((Left a) : xs) (as, bs) = go xs (as ++ [a], bs)
      go ((Right b) : xs) (as, bs) = go xs (as, bs ++ [b]) 



eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe' _ (Left _) = Nothing
eitherMaybe' f (Right b) = Just $ f b

either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' f1 _ (Left a) = f1 a
either' _ f2 (Right b) = f2 b

eitherMaybe'' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe'' f e =
  either' (const Nothing) (Just . f) e


--unfolds
myIterate :: (a -> a) -> a -> [a]
myIterate f a = a : myIterate f (f a)

myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a]
myUnfoldr f b =
  case f b of
    Just (a, b') ->
      a : myUnfoldr f b'
    Nothing ->
      []

myIterate' :: (a -> a) -> a -> [a]
myIterate' f a = myUnfoldr (\x -> Just (x, f x)) a


unfoldTree :: (a -> Maybe(a, b, a)) -> a -> BinaryTree b
unfoldTree f a =
  case f a of
    Nothing -> Leaf
    (Just (al, b, ar)) -> Node (unfoldTree f al) b (unfoldTree f ar) 


treeBuild :: Integer -> BinaryTree Integer
treeBuild layersToMake =
  unfoldTree (\x -> makeTreeNode layersToMake x) 0
    where
      makeTreeNode stop current
        | current >= stop = Nothing
        | otherwise = Just (current + 1, current, current + 1)
