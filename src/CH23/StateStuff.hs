{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleContexts #-}

module CH23.StateStuff where

import System.Random
import Control.Monad (replicateM)
import Control.Monad.Trans.State

data Die = One | Two | Three | Four | Five | Six
  deriving (Show, Eq, Ord)

intToDie :: Int -> Die
intToDie int =
  case int of
    1 -> One
    2 -> Two
    3 -> Three
    4 -> Four
    5 -> Five
    6 -> Six
    x -> error $ "bad intToDie" ++ show x


-- doesn't work, always gives the same result
rollThreeTimes :: (Die, Die, Die)
rollThreeTimes = do
  let s = mkStdGen 1
      (d1, s1) = randomR (1, 6) s
      (d2, s2) = randomR (1, 6) s1
      (d3, s3) = randomR (1, 6) s2
  (intToDie d1, intToDie d2, intToDie d3)

rollDie :: State StdGen Die
rollDie = state $ do
  (n, s) <- randomR (1, 6)
  pure (intToDie n, s)

-- but we can fmap over the Die in `State StgGen Die`
--
rollDie' :: State StdGen Die
rollDie' = 
  intToDie <$> state (randomR (1, 6))
  
rollDieThreeTimes' :: State StdGen (Die, Die, Die)
rollDieThreeTimes' =
  (,,) <$> rollDie <*> rollDie <*> rollDie

doThreeTimesRolling = evalState rollDieThreeTimes' (mkStdGen 0)

badNDie :: Int -> StdGen -> [Die]
badNDie int gen =
  take int $ evalState (repeat <$> rollDie') gen

nDie :: Int -> State StdGen [Die]
nDie n = replicateM n rollDie'

-- ghci> badNDie 5 (mkStdGen 1)
-- [Six,Six,Six,Six,Six]
-- ghci> evalState (nDie 5) (mkStdGen 1)
-- [Six,Four,Three,Five,Four]

-- ghci> :t evalState
-- evalState :: State s a -> s -> a
-- evalState is some monad transformer stuff

rollsToGetTwenty :: StdGen -> Int
rollsToGetTwenty g = go 0 0 g
  where
    go :: Int -> Int -> StdGen -> Int
    go sum count gen 
      | sum >= 20 = count
      | otherwise =
        let 
          (die, nextGen) = randomR (1, 6) gen
        in
        go (sum + die) (count + 1) nextGen
          
rollsToGetN :: Int -> StdGen -> Int
rollsToGetN limit g = go 0 0 g
  where
    go :: Int -> Int -> StdGen -> Int
    go sum count gen =
      if sum >= limit 
         then count
         else
              let 
                (die, nextGen) = randomR (1, 6) gen
              in
              go (sum + die) (count + 1) nextGen

rollsLogged :: Int -> StdGen -> (Int, [Die])
rollsLogged limit g = go 0 0 [] g
  where
    go :: Int -> Int -> [Die] -> StdGen -> (Int , [Die])
    go sum count rolls gen 
      | sum >= limit = (count, rolls)
      | otherwise = 
              let 
                (die, nextGen) = randomR (1, 6) gen
              in
              go (sum + die) (count + 1) (rolls ++ [intToDie die]) nextGen



newtype Moi s a = Moi {runMoi :: s -> (a, s)}

instance Functor (Moi s) where
  fmap :: (a -> b) -> Moi s a -> Moi s b
  fmap f (Moi g) =  Moi $ \s -> 
                           let (a, s') = g s
                           in  (f a, s')

instance Applicative (Moi s) where
  pure :: a -> Moi s a
  pure a = Moi $ \s -> (a, s)

  (<*>) :: Moi s (a -> b) -> Moi s a -> Moi s b
  (<*>) (Moi sab) (Moi fa) = Moi $ \s -> let (a, s1) = fa s
                                             (fab, s2) = sab s1
                                         in (fab a, s2)

instance Monad (Moi s) where
  return = pure

  (>>=) :: Moi s a -> (a -> Moi s b) -> Moi s b
  (Moi f) >>= g =
    Moi $ \s -> let (a, s') = f s
                in runMoi (g a) s'

get' :: Moi s s
get' = Moi {runMoi = \x -> (x, x)}

put' :: s -> Moi s ()
put' s = Moi {runMoi = \_ -> ((), s)}

exec' :: Moi s a -> s -> s
exec' (Moi sa) s =
  snd $ sa s

eval' :: Moi s a -> s -> a
eval' (Moi sa) s = 
  fst $ sa s

modify' :: (s -> s) -> Moi s ()
modify' f =
  Moi (\s -> ((), f s))


statefulFizzBuzz :: IO ()
statefulFizzBuzz =
  mapM_ putStrLn $ reverse $ fizzbuzzList [1..100]


fizzbuzzList :: [Integer] -> [String]
fizzbuzzList list =
  execState (mapM_ addResult list) []

addResult :: Integer -> State [String] ()
addResult n = do
  xs <- get
  let result = fizzBuzz n
  put (result : xs)

fizzBuzz :: Integer -> String
fizzBuzz n | n `mod` 15 == 0 = "FizzBuzz"
           | n `mod` 5 == 0 = "Buzz"
           | n `mod` 3 == 0 = "Fizz"
           | otherwise = show n

fizzbuzzFromTo :: Integer -> Integer -> [String]
fizzbuzzFromTo start end =
  execState (mapM_ addResult [end, end - 1..start]) []

testIt :: IO ()
testIt =
  putStrLn $ show (reverse (fizzbuzzList [1..100]) == (fizzbuzzFromTo 1 100))


