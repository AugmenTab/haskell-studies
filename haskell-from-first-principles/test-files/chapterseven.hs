-- chapterseven.hs
module ChapterSeven where


{- 1: The following function returns the tens digit of an integral argument.
    a) First, rewrite it using divMod.
    b) Does the divMod version have the same type as the original version?
        * Yes.
    c) Add a function to get the hundreds digit instead. -}
tensDigit :: Integral a => a -> a
tensDigit x = d
   where xLast = x `div` 10
         d     = xLast `mod` 10


tensDigit' :: Integral a => a -> a
tensDigit' x = fst . divMod x $ 10


hunsDigit :: Integral a => a -> a
hunsDigit x = fst . divMod x $ 100


-- 2: Implement the following function, once using a case expression and once with a guard.
-- case
foldBool1 :: a -> a -> Bool -> a
foldBool1 x y b =
   case b of
     True  -> y
     False -> x

-- guard
foldBool2 :: a -> a -> Bool -> a
foldBool2 x y b
  | b == True  = y
  | b == False = x

-- pattern matching (provided)
foldBool3 :: a -> a -> Bool -> a
foldBool3 x _ False = x
foldBool3 _ y True  = y


-- 3: Fill in the definition.
g :: (a -> b) -> (a, c) -> (b, c)
g f (a, c) = (f a, c)


-- 4, originals shows in comments after the changed lines.
-- 5: Write a point-free version of roundTrip.
-- 6: Change the type of roundTrip to (Show a, Read b) => a -> b, and make it work.
roundTrip :: (Show a, Read b) => a -> b --roundTrip :: (Show a, Read a) => a -> a
roundTrip = \a -> read . show $ a --roundTrip a = read (show a)


arith4 :: IO ()
arith4 = do
  print (roundTrip 4 :: Integer) --print (roundTrip 4)
  print (id 4)
