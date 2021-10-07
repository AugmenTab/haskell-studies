# Chapter 12: Signaling Adversity

### Determine the kinds

1. Given `id :: a -> a`, what is the kind of `a`?
    * `a :: *`
2. Given `r :: a -> f a`, what are the kinds of `a` and `f`?
    * `a :: *`
    * `f :: * -> *`

### String processing

```haskell
{-
    1. Write a recursive function named `replaceThe` that takes a text/string,
    breaks it into words, and replaces each instance of "the" with "a". It
    should only replace exactly the word "the". `notThe` is a suggested helper
    function for accomplishing this.
-}
replaceThe :: String -> String
replaceThe = unwords . map passOrReplace . words
  where passOrReplace x = if notThe x == Nothing then "a" else x

replaceThe' :: String -> String
replaceThe' []         = []
replaceThe' (x:[])     = [x]
replaceThe' (x:y:[])   = [x, y]
replaceThe' (x:y:z:xs) = case notThe [x, y, z] of
    Nothing -> "a" ++ replaceThe' xs
    Just _  -> [x] ++ replaceThe' (y : z : xs)

notThe :: String -> Maybe String
notThe str = if str == "the" then Nothing else Just str

{-
    2. Write a recursive function that takes a text/string, breaks it into
    words, and counts the number of instances of "the" followed by a
    vowel-initial word.
-}
countTheBeforeVowel :: String -> Integer
countTheBeforeVowel = undefined

-- 3. Return the number of letters that are vowels in a word.
countVowels :: String -> Integer
countVowels = toInteger . length . filter (`elem` "aeiou")
```

### Validate the word

Use the `Maybe` type to write a function that counts the number of vowels in a string and the number of consonants. If the number of vowels exceeds the number of consonants, the function returns `Nothing`. In many human languages, vowels rarely exceed the number of consonants, so when they do, it *may* indicate the input isn't a word (that is, a valid input to your dataset).

```haskell
newtype Word' = Word' String deriving (Eq, Show)

vowels :: String
vowels = "aeiouAEIOU"

mkWord :: String -> Maybe Word'
mkWord string
    | v > c     = Nothing
    | otherwise = Just $ Word' string
  where c = length $ filter (not . (`elem` vowels)) string
        v = length $ filter (`elem` vowels) string
```

### It's only natural

You'll be presented with a datatype to represent the natural numbers. The only values representable with the naturals are whole numbers from zero to infinity. Your task will be to implement functions to convert natural numbers to integers and integers to naturals. The conversion from `Nat` to `Integer` won't return `Maybe`, because - as you know - `Integer` is a strict superset of `Nat`. Any `Nat` can be represented by an `Integer`, but the same is *not* true of any `Integer`. Negative numbers are not valid natural numbers.

```haskell
data Nat
    = Zero
    | Succ Nat
    deriving (Eq, Show)

natToInteger :: Nat -> Integer
natToInteger Zero     = 0
natToInteger (Succ x) = 1 + (natToInteger x)

integerToNat :: Integer -> Maybe Nat
integerToNat n
    | n <  0    = Nothing
    | n == 0    = Just Zero
    | otherwise =
        case integerToNat (n - 1) of
            Nothing -> Nothing
            Just x  -> Just (Succ x)
```

### Small library for Maybe

Write the following functions.

```haskell
-- 1. Simple Boolean checks for `Maybe` values.
isJust :: Maybe a -> Bool
isJust (Just _) = True
isJust Nothing  = False

isNothing :: Maybe a -> Bool
isNothing = not . isJust

{-
    2. The following is the `Maybe` catamorphism. You can turn a `Maybe` value
    into anything else with this.
-}
mayybee :: b -> (a -> b) -> Maybe a -> b
mayybee b f (Just a) = f a
mayybee b f Nothing  = b

{-
    3. In case you just want to provide a fallback value. Try writing it in
    terms of the `Maybe` catamorphism.
-}
fromMaybe :: a -> Maybe a -> a
fromMaybe a (Just a') = a'
fromMaybe a Nothing   = a

-- 4. Converting between `List` and `Maybe`.
listToMaybe :: [a] -> Maybe a
listToMaybe []     = Nothing
listToMaybe (x:xs) = Just x

maybeToList :: Maybe a -> [a]
maybeToList Nothing  = []
maybeToList (Just a) = [a]

-- 5. For when we want to drop the `Nothing` values from a list:
catMaybes :: [Maybe a] -> [a]
catMaybes = foldr checkMaybe []
  where checkMaybe Nothing  acc = acc
        checkMaybe (Just x) acc = x : acc

-- 6. You'll see this called `sequence` later.
flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe = foldr checkMaybe (Just [])
  where checkMaybe _        Nothing    = Nothing
        checkMaybe Nothing  _          = Nothing
        checkMaybe (Just x) (Just acc) = Just (x : acc)
```

### Small library for Either

Write each of the following functions. If more than one possible unique function exists for the type, use common sense to determine what it should do.

```haskell
lefts' :: [Either a b] -> [a]
lefts' = foldr checkEither []
  where checkEither (Left  a) acc = a : acc
        checkEither (Right _) acc = acc

rights' ::[Either a b] -> [b]
rights' = foldr checkEither []
  where checkEither (Left  _) acc = acc
        checkEither (Right b) acc = b : acc

partitionEithers' :: [Either a b] -> ([a], [b])
partitionEithers' xs = (lefts' xs, rights' xs)

partitionEithers'' :: [Either a b] -> ([a], [b])
partitionEithers'' = foldr checkEithers ([], [])
  where checkEithers (Left  x) (a, b) = (x : a, b)
        checkEithers (Right x) (a, b) = (a, x : b)

eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe' f (Right b) = Just $ f b
eitherMaybe' f _         = Nothing

either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' fa _  (Left  a) = fa a
either' _  fb (Right b) = fb b

-- Same as before, but use the `either'` function you just wrote.
eitherMaybe'' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe'' f = either' (const Nothing) (Just . f)
```

### Unfolds

```haskell
-- 1. Write the function `myIterate` using direct recursion.
myIterate :: (a -> a) -> a -> [a]
myIterate = undefined

-- 2. Write the function `myUnfoldr` using direct recursion.
myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a]
myUnfoldr = undefined

-- 3. Rewrite `myIterate` into `betterIterate` using `myUnfoldr`.
betterIterate :: (a -> a) -> a -> [a]
betterIterate = undefined
```

### Finally something other than a list!

```haskell
data BinaryTree a
    = Leaf
    | Node (BinaryTree a) a (BinaryTree a)

-- 1. Write `unfold` for `BinaryTree`.
unfold :: (a -> Maybe (a, b, a)) -> a -> BinaryTree b
unfold = undefined

-- 2. Make a tree builder.
treeBuild :: Integer -> BinaryTree Integer
treeBuild n = undefined
```
