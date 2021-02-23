-- FunctionWithLetAndWhere.hs
module FunctionWithLetAndWhere where


printInc :: Int -> IO()
printInc n = print plusTwo
  where plusTwo = n + 2


printInc2 :: Int -> IO()
printInc2 n =
  let plusTwo = n + 2
  in print plusTwo


rewrite1 :: Int
rewrite1  = x * 3 + y
  where x = 3
        y = 1000


rewrite2 :: Int
rewrite2  = x * 5
  where y = 10
        x = 10 * 5 + y


rewrite3 :: Float
rewrite3  = z / x + y
  where x = 7
        y = negate x
        z = y * 10


waxOn :: Int
waxOn     = x * 5
  where z = 7
        y = z + 8
        x = y ^ 2


triple :: Int -> Int
triple x = x * 3


waxOff :: Int -> Int
waxOff x = div ((^2) $ triple x) 10
