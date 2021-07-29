module CH05.Sing where

fstString :: String -> String
fstString x = x ++ " in the rain"

sndString :: [Char] -> [Char]
sndString x = x ++ " over the rainbow"

sing = if (length x < length y) then
                  fstString x
                  else 
                  sndString y
                    where x = "Singin"
                          y = "Somewhere"
