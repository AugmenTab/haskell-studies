# Chapter 08:

### Intermission: Write out the evalutation

```haskell
applyTimes :: (Eq a, Num a) => a -> (b -> b) -> b -> b
applyTimes 0 f b = b
applyTimes n f b = f . applyTimes (n - 1) f $ b


incTimes :: (Eq a, Num a) => a -> a -> a
incTimes times n = applyTimes times (+1) n
```

1. `applyTimes 5 (+1) 5`
    * (+1) . applyTimes (5 - 1) (+1) $ 5
    * (+1) . (+1) . applyTimes (4 - 1) (+1) $ 5
    * (+1) . (+1) . (+1) . applyTimes (3 - 1) (+1) $ 5
    * (+1) . (+1) . (+1) . (+1) . applyTimes (2 - 1) (+1) $ 5
    * (+1) . (+1) . (+1) . (+1) . (+1) . applyTimes (1 - 1) (+1) $ 5
    * (+1) . (+1) . (+1) . (+1) . (+1) . applyTimes 0 (+1) 5
    * (+1) . (+1) . (+1) . (+1) . (+1) 5
    * (+1) . (+1) . (+1) . (+1) 6
    * (+1) . (+1) . (+1) 7
    * (+1) . (+1) 8
    * (+1) 9
    * 10

### Review of types

1. What is the type of `[[True, False], [True, True], [False, True]]`?
   * d
2. Which of the following has the same type as `[[True, False], [True, True], [False, True]]`?
   * b
3. For the function `func x y = x ++ y :: [a] -> [a] -> [a]`, which of the following statements are true?
   * d
4. For the `func` code above, which is a valid application of `func` to both of its arguments?
   * b
    
### Reviewing currying: What is the value of the following expressions based on the code below?

```haskell
cattyConny :: String -> String -> String
cattyConny x y = x ++ " mrow " ++ y

flippy :: String -> String -> String
flippy = flip cattyConny

appedCatty :: String -> String
appedCatty = cattyConny "woops"

frappe :: String -> String
frappe = flippy "haha"
```

1. `appedCatty "woohoo!"`
   * `"woops mrow woohoo!"`
2. `frappe "1"`
   * `"1 mrow haha"`
3. `frappe (appedCatty "2")`
   * `"woops mrow 2 mrow haha"`
4. `appedCatty (frappe "blue")`
   * `"woops mrow blue mrow haha"`
5. `cattyConny (frappe "pink") (cattyConny "green" (apedCatty "blue"))`
   * `"pink mrow haha mrow green mrow woops mrow blue"`
6. `cattyConny (flippy "Pugs" "are") "awesome"`
   * `"are mrow Pugs mrow awesome"`

### Recursion

```haskell
{- 1: Write out the steps for reducing `divideBy 15 2` to its final answer according to the Haskell
code. -}
dividedBy :: Integral a => a -> a -> (a, a)
dividedBy num denom = go num denom 0
  where go n d count
          | n < d     = (count, n)
          | otherwise = go (n - d) d (count + 1)

dividedBy 15 2 = go 15 2 0
                 go (15 - 2) 2 1
                 go (13 - 2) 2 2
                 go (11 - 2) 2 3
                 go (9 - 2)  2 4
                 go (7 - 2)  2 5
                 go (5 - 2)  2 6
                 go (3 - 2)  2 7
                 (7, 1)

{- 2: Write a function that recursively sums all numbers from 1 to n, n being the argument. So if n
is 5, you'd add 1 + 2 + 3 + 4 + 5 to get 150. The type should be `(Eq a, Num a) => a -> a`. -}
sumTo :: (Eq a, Num a) => a -> a -> a
sumTo n = if n == 0 then 0 else n + sumTo (n - 1)

{- 3: Write a function that multiples two integral numbers using recursive summation. The type
should be `(Integral a) => a -> a -> a`. -}
multWithSum :: Integral a => a -> a -> a
multWithSum x y = go x 0 0
  where go a b count
          | count == y = b
          | otherwise  = go a (b + a) (count + 1) 
```

### Fixing dividedBy: Fix `dividedBy` to handle negative numbers and zeroes.

```haskell
data DividedResult
  = Result Integer
  | DividedByZero
  deriving Show

dividedBy :: Integral a => a -> a -> DividedResult
dividedBy 0   _     = Result (0, 0)
dividedBy _   0     = DividedByZero
dividedBy num denom = go absNum absDenom 0
  where multiplier = signum num * signum denom
        absNum     = abs num
        absDenom   = abs denom
        go n d count
          | n < d     = Result (count * multiplier, n)
          | otherwise = go (n - d) d (count + 1)
```

### McCarthy 91 function

```haskell
mccarthy :: Integral a => a -> a
mccarthy x
  | x > 100   = x - 10
  | otherwise = mccarthy (mccarthy (x + 11))
```

### Numbers into words

```haskell
import Data.List (intercalate)

digitToWord :: Int -> String
digitToWord n = intercalate "-" (map wordNumber (digits n))
  
digits :: Int -> [Int]
digits n
  | n < 10    = [n]
  | otherwise = digits (n `div` 10) ++ [n `mod` 10]

wordNumber :: Int -> String
wordNumber 1 = "one"
wordNumber 2 = "two"
wordNumber 3 = "three"
wordNumber 4 = "four"
wordNumber 5 = "five"
wordNumber 6 = "six"
wordNumber 7 = "seven"
wordNumber 8 = "eight"
wordNumber 9 = "nine"
wordNumber _ = "zero"
```
