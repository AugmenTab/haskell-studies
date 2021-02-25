-- test.hs
module Test where


sayHello :: String -> IO ()
sayHello x =
  putStrLn ("Hello, " ++ x ++ "!")


triple :: Integer -> Integer
triple x =
  x * 3


pow :: Integer -> Integer -> Integer
pow x y =
  x ^ y
