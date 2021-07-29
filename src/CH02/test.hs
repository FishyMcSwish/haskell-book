module CH02.Test where

sayHello :: String -> IO ()
sayHello x = 
  putStrLn ("Hello, " ++ x ++ "!")

triple x = x * 3
