# Chapter 09: Lists

### EnumFromTo: Write your own definitions for the types provided.

The solution for this one was taken from GitHub user [nackjicholson](https://github.com/nackjicholson/haskellbook-solutions/blob/master/chapter9/exercises.hs). I had to go looking for guidance on this one for the same reason they specified: the authors of the book want the reader to do this exercise without using range syntax, which is really the only topic this chapter has covered thus far.

```haskell
eftBool :: Bool -> Bool -> [Bool]
eftBool False False = [False]
eftBool True True   = [True]
eftBool False True  = [False, True]
eftBool True False  = []

eft :: (Enum a, Ord a) => a -> a -> [a]
eft start stop
  | start > stop = []
  | otherwise    = start : eft (succ start) stop

eftOrd :: Ordering -> Ordering -> [Ordering]
eftOrd = eft

eftInt :: Int -> Int -> [Int]
eftInt = eft

eftChar :: Char -> Char -> [Char]
eftChar = eft
```

### Thy fearful symmetry

```haskell
{- 1: Using takeWhile and dropWhile, write a function that takes a string and returns a list of
strings, using spaces to separate the elements of the string into words, as in the following sample:
myWords "sheryl wants fun" => ["sheryl", "wants", "fun"]. -}
myWords :: String -> [String]
myWords [] = []
myWords (' ' : s) = myWords s
myWords s = takeWhile (/=' ') s : myWords (dropWhile (/=' ') s)

{- 2: Write a function that takes a string and returns a list of strings, using newline separators
to break up the string. -}
firstSen = "Tyger Tyger, burning bright\n"
secondSen = "In the forests of the night\n"
thirdSen = "What immortal hand or eye\n"
fourthSen = "Could frame thy fearful\
           \ symmetry?"
sentences = firstSen ++ secondSen ++ thirdSen ++ fourthSen

myLines :: String -> String
myLines "" = []
myLines ('\n' : s) = myLines s
myLines s = takeWhile (/='\n') s : myLines (dropWhile (/='\n') s)

shouldEqual :: [String]
shouldEqual =
  [ "Tyger Tyger, burning bright"
  , "In the forests of the night"
  , "What immortal hand or eye"
  , "Could frame thy fearful symmetry?"
  ]

main :: IO ()
main = print $ "Are they equal? " ++ show (myLines sentences == shouldEqual)

{- 3: Write a new function that parameterizes the character you're breaking the string argument on
and rewrite myWords and myLines using that parameter. -}
mySplitter :: Char -> String -> [String]
mySplitter _ "" = []
mySplitter c s = keep : mySplitter c rest
  where keep = takeWhile (/= c) s
        rest = drop (length keep + 1) s
```

### Comprehend thy lists: Determine what you think the output lists will be given the following functions.

```haskell
mySqr :: (Num a, Enum a) => [a]
mySqr = [x^2 | x <- [1..10]]

-- 1
[x | x <- mySqr, rem x 2 == 0]

-- 2
[(x, y) | x <- mySqr
        , y <- mySqr
        , x < 50, y > 50
]

-- 3
take 5 [(x, y) | x <- mySqr
               , y <- mySqr
               , x < 50, y > 50
       ]
```

1. `[4, 16, 36, 64, 100]`
2. `[(1,64),(1,81),(1,100),(4,64),(4,81),(4,100),(9,64),(9,81),(9,100),(16,64),(16,81),(16,100),(25,64),(25,81),(25,100),(36,64),(36,81),(36,100),(49,64),(49,81),(49,100)]`
3. `[(1, 64), (1, 81), (1, 100), (4, 64), (4, 81)]`

### Square cube

```haskell
mySqr = [x^2 | x <- [1..5]]
myCube = [y^3 | y <- [1..5]]
```

1. Write an expression that will make tuples of the outputs of `mySqr` and `myCube`.
   * `[(x, y) | x <- mySqr, y <- myCube]`
2. Alter that expression so that it only uses the `x` and `y` values that are less than 50.
   * `[(x, y) | x <- mySqr, y <- myCube, x < 50, y < 50]`
3. Apply another function to that list comprehension to determine how many tuples inhabit your output list.
   * `length [(x, y) | x <- mySqr, y <- myCube, x < 50, y < 50]`

### Bottom madness: Will the following expressions return a value or be &bot;?

1. `[x^y | x <- [1..5], y <- [2, undefined]]`
   * &bot;
2. `take 1 $ [x^y | x <- [1..5], y <- [2, undefined]]`
   * `[1]`
3. `sum [1, 2, undefined]`
   * &bot;
4. `length [1, 2, undefined]`
   * 3
5. `length $ [1, 2, 3] ++ undefined`
   * &bot;
6. `take 1 $ filter even [1, 2, 3, undefined]`
   * `[2]`
7. `take 1 $ filter even [1, 3, undefined]`
   * &bot;
8. `take 1 $ filter odd [1, 3, undefined]`
   * `[1]`
9. `take 2 $ filter odd [1, 3, undefined]`
   * ` [1, 3]`
10. `take 3 $ filter odd [1, 3, undefined]`
    * &bot;

### Is it normal form?: For each expression below, determine whether it is normal form, weak head normal form, or neither.

1. `[1, 2, 3, 4, 5]`
   * Normal Form
2. `1 : 2 : 3 : 4 : _`
   * Weak Head Normal Form
3. `enumFromTo 1 10`
   * Neither
4. `length [1, 2, 3, 4, 5]`
   * Normal Form
5. `sum (enumFromTo 1 10)`
   * Normal Form
6. `['a'..'m'] ++ ['n'..'z']`
   * Neither
7. `(_, 'b')`
   * Weak Head Normal Form

### More bottoms

1. Will `take 1 $ map (+1) [undefined, 2, 3]` return a value or be &bot;?
   * &bot;
2. Will `take 1 $ map (+1) [1, undefined, 3]` return a value?
   * `[2]`
3. Will `take 1 $ map (+1) [1, 2, undefined]` return a value?
   * `[2]`
4. What does the function `itIsMystery xs = map (\x -> elem x "aeiou") xs` do? What is its type?
    * It creates a list of booleans reflecting whether each element in the list argument is a vowel.
    * itIsMystery :: [Char] -> [Bool]
5. What will be the result of the following functions?
    1. `map (^2) [1..10]`
       * `[1, 4, 9, 16, 25, 36, 49, 64, 81, 100]`
    2. `map minimum [[1..10], [10..20], [20..30]]`
       * `[1, 10, 20]`
    3. `map sum [[1..5], [1..5], [1..5]]`
       * `[15, 15, 15]`
6. Rewrite `map (\x -> if x == 3 then (-x) else (x)) [1..10]` using `Data.Bool (bool)`.
   * `map (\x -> bool x (-x) (x == 3)) [1..10]`

### Filtering

1. How might we write a filter function that would give us all the multiples of 3 out of a list from 1-30?
    * `filter (\x -> rem x 3 == 0) [1..30]`
    * `[x | x <- [1..30], rem x 3 == 0]`
2. How could we compose the above function(s) with the `length` function to tell us *how many* multiples of 3 there are between 1 and 30?
    * `length $ filter (\x -> rem x 3 == 0) [1..30]`
    * `length $ [x | x <- [1..30], rem x 3 == 0]`
3. Write a function that removes all articles ("the", "a", and "an") from sentences.
    * `stripArticles xs = filter (\x -> not (elem x ["the", "a", "an"])) (words xs)`
    * `stripArticles xs = [x | x <- words xs, not $ elem x ["the", "a", "an"]]`

### Zipping

```haskell
-- 1: Write your own version of zip, and ensure it behaves the same as the original.
myZip :: [a] -> [b] -> [(a, b)]
myZip []     _      = []
myZip _      []     = []
myZip (x:xs) (y:ys) = (x, y) : myZip xs ys

-- 2: Do the same for zipWith.
myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith _ []     _      = []
myZipWith _ _      []     = []
myZipWith f (x:xs) (y:ys) = f x y : myZipWith f xs ys

-- 3: Rewrite zip in terms of the zipWith you wrote.
zipAgain :: [a] -> [b] -> [(a, b)]
zipAgain = myZipWith (\x y -> (x, y))
```

### Data.Char

1. Query the types of `isUpper` and `toUpper`.
    * `isUpper :: Char -> Bool`
    * `toUpper :: Char -> Char`
2. Write a function that filters all the uppercase letters out of a `String`.
    * `removeUpper = filter isUpper`
    * The examples given in the book imply that the above is correct (`"HbEfLrLxO" => "HELLO"`), but the description given should mean we filter out the uppercase, leaving only lowercase letters as the result. That version would be: `removeUpper xs = [x | xs, not (isUpper x)]`.
3. Write a function that will capitalize the first letter of a string and return the entire string.
   * `capitalize (x:xs) = toUpper x : xs`
4. Make a new version of the above function that is recursive, and capitalizes every letter in a string.
    * See below.
    * Alternate: `capitalizeAll = map toUpper`
5. Write a function that will capitalize the first letter of a `String` and return only that letter as a result.
   * `getCapital x = toUpper (head x)`
6. Rewrite the above as a composed function and as a point-free function.
    * `getCapital x = toUpper $ head x`
    * `getCapital = toUpper . head`

```haskell
-- 4
capitalizeAll :: [Char] -> [Char]
capitalizeAll ""     = ""
capitalizeAll (x:xs) = toUpper x : capitalizeAll xs
```

### Ciphers

[Cipher.hs](../test-files/Cipher.hs)
