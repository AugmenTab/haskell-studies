-- chapter01: Starting Out
module Chapter01 where


doubleMe :: Num a => a -> a
doubleMe x = x + x

doubleUs :: Num a => a -> a -> a
doubleUs x y = x * 2 + y * 2

doubleSmallNumber :: (Ord a, Num a) => a -> a
doubleSmallNumber x = if x > 100 then x else x * 2

doubleSmallNumber' :: (Ord a, Num a) => a -> a
doubleSmallNumber' x = (if x > 100 then x else x * 2) + 1

conanO'Brien :: [Char]
conanO'Brien = "It's a-me, Conan O'Brien!"

boomBangs :: Integral a => [a] -> [String]
boomBangs xs = [if x < 10 then "BOOM!" else "BANG!" | x <- xs, odd x]

length' :: Integral a => [*] -> a
length' xs = sum [1 | _ <- xs]

removeNonUppercase :: String -> String
removeNonUppercase st = [c | c <- st, elem c ['A'..'Z']]
