-- FunctionWithLetAndWhere.hs
module FunctionWithLetAndWhere where


printInc n = print plusTwo
  where plusTwo = n + 2


printInc2 n =
  let plusTwo = n + 2
  in print plusTwo


rewrite1  = x * 3 + y
  where x = 3
        y = 1000


rewrite2  = x * 5
  where y = 10
        x = 10 * 5 + y


rewrite3  = z / x + y
  where x = 7
        y = negate x
        z = y * 10


waxOn     = x * 5
  where z = 7
        y = z + 8
        x = y ^ 2


triple x = x * 3


waxOff x = div ((^2) $ triple x) 10
