-- FunctionWithLetAndWhere.hs
module FunctionWithLetAndWhere where


printInc :: Integer -> IO()
printInc n = print plusTwo
  where plusTwo = n + 2


printInc2 :: Integer -> IO()
printInc2 n =
  let plusTwo = n + 2
  in print plusTwo


rewrite1 :: Integer
rewrite1  = x * 3 + y
  where x = 3
        y = 1000


rewrite2 :: Integer
rewrite2  = x * 5
  where y = 10
        x = 10 * 5 + y


rewrite3 :: Double
rewrite3  = z / x + y
  where x = 7
        y = negate x
        z = y * 10


waxOn :: Integer
waxOn     = x * 5
  where z = 7
        y = z + 8
        x = y ^ 2


triple :: Integer -> Integer
triple x = x * 3


waxOff :: Int -> Int
waxOff x = div ((^2) $ triple x) 10
