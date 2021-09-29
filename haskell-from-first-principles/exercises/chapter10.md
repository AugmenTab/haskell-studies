# Chapter 10: Folding Lists

### Understanding folds

1. `foldr (*) 1 [1..5]` will return the same result as which of the following?
    * b, c
2. Write out the evaluation steps for: `foldl (flip (*)) 1 [1..3]`
    * (((1 * 1) * 2) * 3)
    * ((1 * 2) * 3)
    * (2 * 3)
    * 6
3. One difference between `foldr` and `foldl` is:
    * c
4. Folds are catamorphisms, which means they are generally used to:
    * a
5. The following are simple folds very similar to what you've already seen, but each has at least one error. Please fix and test them in your REPL:
    1. `foldr (++) ["woot", "WOOT", "woot"]`
        * Missing the accumulator. `foldr (++) "" ["woot", "WOOT", "woot"]`
    2. `foldr max [] "fear is the little death"`
        * An empty list and a `Char` cannot be compared. Changed accumulator to the lowest `Char` value. `foldr max (minBound :: Char) "fear is the little death"`
    3. `foldr and True [False, True]`
        * `and` is a function that takes a list of `Bool`. Comparing two `Bool`s requires the `&&` operator. `foldr (&&) True [False, True]`
    4. `foldr (||) True [False, True]`
        * This question specifically asks, "can it ever return a different answer?" It cannot, because the accumulator is `True`. This means the first comparison will always return `True`, and `True || _` will always be `True`, so all further comparisons will also be `True`.
    5. `foldl ((++) . show) "" [1..5]`
        * `foldl` passes the accumulator first, then the head of the list, to the function it uses to fold. In this case, `show` needs the head of the list. Using `foldr`, which takes the head and then the accumulator, solves this problem. `foldr ((++) . show) "" [1..5]`
    6. `foldr const 'a' [1..5]`
        * Because `const` always returns the first argument passed to it, and because the first argument passed with a `foldr` is the head of the list, const is going to return `1`. Since we've already stated that the accumulator is a `Char`, this will not compile. We instead need to use a `foldl` so that the accumulator is passed first. `foldl const 'a' [1..5]`
        * Alternatively, we can flip the arguments to `const`, like so: `foldr (flip const) 'a' [1..5]`
    7. `foldr const 0 "tacos"`
        * Same reason as #6. `foldl const 0 "tacos"`
    8. `foldl (flip const) 0 "burritos"`
        * The arguments to `const` are flipped, so we are getting the head of the list first again. This causes the same problems as it did in #6. Here, we can switch to a `foldr` like this: `foldr (flip const) 0 "burritos"`
        * Alternatively, we can just stop flipping the arguments: `foldl const 0 "burritos"`
    9. `foldl (flip const) 'z' [1..5]`
        * Same reason as #8. `foldr (flip const) 'z' [1..5]`
        * And we can stop flipping, too: `foldl const 'z' [1..5]`


### Database processing

```haskell
import qualified Data.Time as T

data DatabaseItem
    = DbString String
    | DbNumber Integer
    | DbDate   T.UTCTime
    deriving (Eq, Ord, Show)

theDatabase :: [DatabaseItem]
theDatabase =
    [ DbDate (T.UTCTime (T.fromGregorian 1911 5 1) (T.secondsToDiffTime 34123))
    , DbNumber 9001
    , DbString "Hello, world!"
    , DbDate (T.UTCTime (T.fromGregorian 1921 5 1) (T.secondsToDiffTime 34123))
    ]

{- 1. Write a function that filters for DbDate values and returns a list of the
UTCTime values inside them. -}
filterDbDate :: [DatabaseItem] -> [T.UTCTime]
filterDbDate = foldr times []
  where times (DbDate t) acc = t : acc
        times _          acc = acc

{- 2. Write a function that filters for DbNumber values and returns a list of
the Integer values inside them. -}
filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber = foldr nums []
  where nums (DbNumber n) ns = n : ns
        nums _            ns = ns

-- 3. Write a function that gets the most recent date.
mostRecent :: [DatabaseItem] -> T.UTCTime
mostRecent = maximum . filterDbDate

-- 4. Write a function that sums all of the DbNumber values.
sumDb :: [DatabaseItem] -> Integer
sumDb = sum . filterDbNumber

-- 5. Write a function that gets the average of the DbNumber values.
avgDb :: [DatabaseItem] -> Double
avgDb xs = if len > 0 then summed / len else 0
  where summed = fromIntegral $ sumDb xs
        len    = fromIntegral $ length $ filterDbNumber xs
```

### Scans exercises

```haskell
fibs    = 1 : scanl (+) 1 fibs
fibsN x = fibs !! x
```

1. Modify your `fibs` function to only return the first 20 Fibonacci numbers.
    * `fibs = take 20 $ 1 : scanl (+) 1 fibs`
2. Modify `fibs` to return the Fibonacci numbers that are less than 100.
    * `fibs = takeWhile (< 100) $ 1 : scanl (+) 1 fibs`
3. Try to write the `factorial` function from Chapter 8 as a scan. You'll want `scanl` again, and your start value will be 1.
    * `factorial = (!!) (scanl (*) 1 [1..])`

### Warm-up and review

```haskell
{- 1. Given the following set of consonants and vowels: 

    stops  = "pbtdkg"
    vowels = "aeiou"

    a. Write a function that takes inputs from stops and vowels and makes
    3-tuples of all possible stop-vowel-stop combinations. These will not all
    correspond to real words in English, although the stop-vowel-stop pattern is
    common enough that many of them will.

    b. Modify that function so that it only returns the combinations that begin
    with a p.

    c. Now set up lists of nouns and verbs (instead of stops and vowels), and
    modify the function to make tuples representing possible noun-verb-noun
    sentences.
-}
svsWords :: [(Char, Char, Char)]
svsWords stops vowels = [ (x, y, z) | x <- stops, y <- vowels, z <- stops ]

svsWordsApp :: [(Char, Char, Char)]
svsWordsApp stops vowels = (,,) <$> stops <*> vowels <*> stops

svsWordsOnlyP :: [(Char, Char, Char)]
svsWordsOnlyP stops vowels = [ ('p', y, z) | y <- vowels, z <- stops ]

svsWordsOnlyPApp :: [(Char, Char, Char)]
svsWordsOnlyPApp stops vowels = (,,) <$> "p" <*> vowels <*> stops

nvnSentences :: [("String", "String", "String")]
nvnSentences nouns verbs = [ (x, y, z) | x <- nouns, y <- verbs, z <- nouns ]

nvnSentencesApp :: [("String", "String", "String")]
nvnSentencesApp nouns verbs = (,,) <$> nouns <*> verbs <$> nouns

-- 2. What does the following mystery function do? What is its type?
seekritFunc :: String -> Int
seekritFunc x = div (sum (map length (words x))) (length (words x))

{- It finds the average word length in a given String. 
    (sum (map length (words x))) gets the total number of characters.
    (length (words x)) gets the number of words.
    div then divides the number of characters by the number of words.
-}

-- 3. Rewrite seekritFunc to use fractional division.
seekritFuncFrac :: String -> Double
seekritFuncFrac x = numChars / numWords
  where numChars = fromIntegral (sum (map length (words x)))
        numWords = fromIntegral (length (words x))
```

### Rewriting functions using folds

```haskell
myOr :: [Bool] -> Bool
myOr = foldr (||) False

myAny :: (a -> Bool) -> [a] -> Bool
myAny f = foldr ((||) . f) False

myElem :: Eq a => a -> [a] -> Bool
myElem e = foldr ((||) . (== e)) False

myElem' :: Eq a => a -> [a] -> Bool
myElem' e = any (== e)

myReverse :: [a] -> [a]
myReverse = foldr (\x acc -> acc ++ [x]) []

myReverse' :: [a] -> [a]
myReverse' = foldl (flip (:)) []

myMap :: (a -> b) -> [a] -> [b]
myMap f = foldr (\x acc -> f x : acc) []

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f = foldr (\x acc -> if f x then x : acc else acc) []

squish :: [[a]] -> [a]
squish = foldr (++) []

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f = foldr ((++) . f) []

squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy _ []     = undefined
myMaximumBy f (x:xs) = foldl (\x acc -> if f x acc == GT then x else acc) x xs

myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy _ []     = undefined
myMinimumBy f (x:xs) = foldl (\x acc -> if f x acc == LT then x else acc) x xs
```
