# Chapter 09: Lists

### EnumFromTo: Write your own definitions for the types provided.

The solution for this one was taken from GitHub user [nackjicholson](https://github.com/nackjicholson/haskellbook-solutions/blob/master/chapter9/exercises.hs). I had to go looking for guidance on this one for the same reason they specified: the authors of the book want the reader to do this exercises without using range syntax, which is really the only topic this chapter has covered thus far.

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

