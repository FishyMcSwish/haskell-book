module CH13.Exercises where

import Control.Monad
import System.Exit (exitSuccess)
import Data.Char

palindrome :: IO ()
palindrome = forever $ do
    line1 <- getLine
    case clean line1 == reverse (clean line1) of
      True -> putStrLn "it's a palindrome"
      False -> 
        do
          putStrLn "nope!"
          exitSuccess
    where
      clean = filter isAlpha . map toUpper


type Name = String
type Age = Integer
data Person = Person Name Age deriving Show

data PersonInvalid = NameEmpty 
                   | AgeTooLow 
                   | PersonInvalidUnknown String 
                   deriving (Eq, Show)


mkPerson :: Name -> Age -> Either PersonInvalid Person
mkPerson name age
  | name /= "" && age > 0 = Right $ Person name age
  | not (age > 0) = Left AgeTooLow
  | otherwise = Left $ PersonInvalidUnknown $
    "Name was: " ++ show name ++ " Age was" ++ show age




personProgram :: IO ()
personProgram = do
  putStrLn "age: "
  age <- getLine
  putStrLn "name: "
  name <- getLine
  case mkPerson name (read age) of
    Left invalidPerson -> 
      putStrLn $ show invalidPerson
    Right person ->
      putStrLn $ "Yay! got a person: " ++ show person
        
