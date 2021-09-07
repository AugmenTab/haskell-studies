-- chapter03: Syntax in Functions
module Chapter03 where


lucky :: Int -> String
lucky 7 = "LUCKY NUMBER SEVEN!"
lucky _ = "Sorry, you're out of luck, pal!"

sayMe :: Int -> String
sayMe 1 = "One!"
sayMe 2 = "Two!"
sayMe 3 = "Three!"
sayMe 4 = "Four!"
sayMe 5 = "Five!"
sayMe _ = "Not between 1 and 5."

factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n - 1)

charName :: Char -> String
charName 'a' = "Albert"
charName 'b' = "Broseph"
charName 'c' = "Cecil"
charName _   = "This Char has no name."

addVectors :: (Double, Double) -> (Double, Double) -> (Double, Double)
addVectors (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

first :: (a, b, c) -> a
first (x, _, _) = x

second :: (a, b, c) -> b
second (_, y, _) = y

third :: (a, b, c) -> c
third (_, _, z) = z

head' :: [a] -> a
head' []    = error "Can't call head on an empty list, dummy!"
head' (x:_) = x

showTwo :: (Show a) => a -> a -> String
showTwo x y= show x ++ " and " ++ show y ++ "."

tell :: (Show a) => [a] -> String
tell []       = "The list is empty."
tell (x:[])   = "The list has one element: " ++ show x ++ "."
tell (x:y:[]) = "The list has two elements: " ++ showTwo x y
tell (x:y:_)  = "This list is long. The first two elements are: " ++ showTwo x y

badAdd :: (Num a) => [a] -> a
badAdd (x:y:z:[]) = x + y + z

firstLetter :: String -> String
firstLetter ""         = "Empty string, whoops!"
firstLetter all@(x:xs) = "The first letter of " ++ all ++ " is " ++ [x] ++ "."

bmiTell :: Double -> String
bmiTell bmi
    | bmi <= 18.5 = "You're underweight, eat more!"
    | bmi <= 25.0 = "Looking good!"
    | bmi <= 30.0 = "You're overweight. Let's work out together!"
    | otherwise   = "You're obese. Go see a doctor."

bmiTell' :: Double -> Double -> String
bmiTell' weight height
    | bmi <= 18.5 = "You're underweight, eat more!"
    | bmi <= 25.0 = "Looking good!"
    | bmi <= 30.0 = "You're overweight. Let's work out together!"
    | otherwise   = "You're obese. Go see a doctor."
  where bmi = weight / height ^ 2

max' :: (Ord a) => a -> a -> a
max' a b
    | a <= b    = b
    | otherwise = a

myCompare :: (Ord a) => a -> a -> Ordering
a `myCompare` b
    | a == b    = EQ
    | a <= b    = LT
    | otherwise = GT

bmiTell'' :: Double -> Double -> String
bmiTell'' weight height
    | bmi <= skinny     = "You're underweight, eat more!"
    | bmi <= normal     = "Looking good!"
    | bmi <= overweight = "You're overweight. Let's work out together!"
    | otherwise   = "You're obese. Go see a doctor."
  where bmi                          = weight / height ^ 2
        (skinny, normal, overweight) = (18.5, 25.0, 30.0)

badGreeting :: String
badGreeting = "Oh! Pfft. It's you. "

niceGreeting :: String
niceGreeting = "Hello! So very nice to see you, "

greet :: String -> String
greet "Juan"     = niceGreeting ++ "Juan!"
greet "Fernando" = niceGreeting ++ "Fernando!"
greet name       = badGreeting ++ name ++ "."

greet' :: String -> String
greet' name
    | name == "Juan" || name == "Fernando" = niceGreeting ++ name ++ "!"
    | otherwise                            = badGreeting ++ name ++ "."

initials :: String -> String -> String
initials firstName lastName = [f] ++ ". " ++ [l] ++ "."
  where (f:_) = firstName
        (l:_) = lastName

calcBMIs :: [(Double, Double)] -> [Double]
calcBMIs xs = [bmi w h | (w, h) <- xs]
  where bmi weight height = weight / height ^ 2

cylinder :: Double -> Double -> Double
cylinder r h =
    let sideArea = 2 * pi * r * h
        topArea  = pi * r * h
    in  sideArea + 2 * topArea

calcBMIs' :: [(Double, Double)] -> [Double]
calcBMIs' xs = [bmi | (w, h) <- xs, let bmi = w / h ^ 2]

calcOverweightBMIs :: [(Double, Double)] -> [Double]
calcOverweightBMIs xs = [bmi | (w, h) <- xs, let bmi = w / h ^ 2, bmi > 25.0]

describeList :: [a] -> String
describeList ls = "The list is " ++ case ls of []  -> "empty."
                                               [x] -> "a singleton list."
                                               xs  -> "a longer list."

describeList' :: [a] -> String
describeList' ls = "The list is " ++ what ls
  where what []  = "empty."
        what [x] = "a singleton list."
        what xs  = "a longer list."
