module CH07.Arith4 where

roundTrip :: (Show a, Read b) => a -> b
roundTrip = read . show

main = do
  print ( (roundTrip 4) :: Int )
  print ( (id 4) :: Int )
