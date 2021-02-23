-- test.hs
module Test where


sayHello :: String -> IO ()
sayHello x =
  putStrLn ("Hello, " ++ x ++ "!")


triple :: Int -> Int
triple x =
  x * 3


pow :: Int -> Int -> Int
pow x y =
  x ^ y
