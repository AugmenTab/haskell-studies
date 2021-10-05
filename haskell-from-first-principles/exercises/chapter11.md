# Chapter 11: Algebraic Datatypes

### Dog types

```haskell
data Doggies a
    = Husky   a
    | Mastiff a
    deriving (Eq, Show)

data DogueDeBordeaux doge = DogueDeBordeaux doge
```

1. type constructor
2. `Doggies :: * -> *`
3. `Doggies String :: *`
4. `Husky 10 :: Num a => Doggies a`
5. `Husky (10 :: Integer) :: Doggies Integer`
6. `Mastiff "Scooby Doo" :: Doggies String`
7. both
8. `DogueDeBordeaux :: doge -> DogueDeBordeaux doge`
9. `DogueDeBordeaux "doggie!" :: DogueDeBordeaux String`

### Vehicles

```haskell
data Price = Price Integer deriving (Eq, Show)

data Manufacturer
    = Mini
    | Mazda
    | Tata
    deriving (Eq, Show)

data Airline
    = PapuAir
    | CatapulsR'Us
    | TakeYourChancesUnited
    deriving (Eq, Show)

data Vehicle
    = Car Manufacturer Price
    | Plane Airline Price
    deriving (Eq, Show)

myCar    = Car Mini  (Price 14000)
urCar    = Car Mazda (Price 20000)
clownCar = Car Tata  (Price 7000)
doge     = Plane PapuAir (Price 419200000)

-- 1. What is the type of myCar?
-- myCar :: Vehicle

-- 2. Define the following functions.
isCar :: Vehicle -> Bool
isCar v = case v of
  (Car _ _) -> True
  _         -> False

isPlane :: Vehicle -> Bool
isPlane = not . isCar

areCars :: [Vehicle] -> [Bool]
areCars = map isCar

-- 3. Write a function to tell us the manufacturer of a piece of data.
getManu :: Vehicle -> Manufacturer
getManu (Car m _) = m

{- 4. Given that we're returning the Manufacturer, what will happen if you use
this on Plane data?

It would create an exception because not every condition has been accounted for.
Instead of a Vehicle -> Manufacturer, we could use a Maybe, like in the getManu'
function I've written below. Another option might be to add a new data NoManu to
the Manufacturer type, which would provide more information than just giving a
Nothing value. -}
getManu' :: Vehicle -> Maybe Manufacturer
getManu' v = case v of
    (Car m _) -> Just m
    _         -> Nothing

{- 5. All right. Let's say you decide to add the size of the plane as an
argument to the Plane constructor. Add that to your datatypes in the appropriate
places, and change your data and functions appropriately. -}
```

### Cardinality

1. 1
2. 3
3. 65,536
4. The cardinality of `Int` is 18,446,744,073,709,551,616. This could also be interpreted as `1 + (maxBound :: Word)`, since `Word` has the same cardinality as the `Int` type, but only contains positive integers. `Integer` values are unbounded.
5. `Int8` is an 8-bit integer, so it stores 2^8 (256) possible values.

### For example

```haskell
data Example = MakeExample deriving Show
```

1. `MakeExample :: Example`. You can't request the type of `Example` because it is a type constructor.
2. When using `:info` on `Example`, you can see the type class instance of `MyExample`.
3. You get `MakeExample :: Int -> Example`, since it has now become a function that constructs an `Example`.

### Logic goats

```haskell
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

class TooMany a where
    tooMany :: a -> Bool

instance TooMany Int where
    tooMany n = n > 42

newtype Goats = Goats Int deriving (Eq, Show, TooMany)

{- 1. Reusing the TooMany type class, write an instance of the type class for
the type (Int, String). -}
instance TooMany (Int, String) where
    tooMany (n, _) = tooMany n

{- 2. Make another TooMany instance for (Int, Int). Sum the values together
under the assumption that this is a count of goats from two fields. -}
instance TooMany (Int, Int) where
    tooMany (x, y) = tooMany (x + y)

{- 3. Make another TooMany instance, this time for (Num a, TooMany a) => (a, a).
This can mean whatever you want, such as summing the two numbers together. -}
instance (Num a, TooMany a) => TooMany (a, a) where
    tooMany (x, y) = tooMany (x + y)
```

### Pity the Bool

```haskell
-- 1. What is the cardinality of the below datatype?
data BigSmall
    = Big   Bool
    | Small Bool
    deriving (Eq, Show)

{- 2. What is the cardinality of NumberOrBool below? What happens if you try to
create a Numba with a numeric literal larger than 127? And with a numeric
literal smaller than -128? -}
import Data.Int

data NumberOrBool
    = Numba     Int8
    | BoolyBool Bool
    deriving (Eq, Show)

myNumba = Numba (-128)
```

1. `BigSmall` has a cardinality of 4. `Bool` has a cardinality of 2, so both `Big` and `Small` would each have a cardinality of 2. Add them together to get 4.
2. `NumberOrBool` has a cardinality of 258. `Numba` has the same cardinality as `Int8`, which is 256. `BoolyBool` has the same cardinality as `Bool`, which is 2. Added together, we get 258. If we try to create a Numba with a numeric literal larger than 127 or smaller than -128, we get a warning that it is out of the range of `Int8`.

### How does your garden grow?

```haskell
data FlowerType
    = Gardenia
    | Daisy
    | Rose
    | Lilac
    deriving Show

type Gardener = String

data Garden = Garden Gardener FlowerType deriving Show

-- 1. What is the sum of products normal form of Garden?
data GardenNormal
    = Gardenia String
    | Daisy    String
    | Rose     String
    | Lilac    String
    deriving Show
```

### Programmers

```haskell
data OperatingSystem
    = GnuPlusLinux
    | OpenBSD
    | Mac
    | Windows
    deriving (Eq, Show)

data ProgLang
    = Haskell
    | Agda
    | Idris
    | PureScript
    deriving (Eq, Show)

data Programmer =
    Programmer { os   :: OperatingSystem
               , lang :: ProgLang
               }
    deriving (Eq, Show)

allOperatingSystems :: [OperatingSystem]
allOperatingSystems =
    [ GnuPlusLinux
    , OpenBSD
    , Mac
    , Windows
    ]

allLanguages :: [ProgLang]
allLanguages = [Haskell, Agda, Idris, PureScript]

{- 1. Write a function that generates all possible values of Programmer. Use the
provided lists of inhabitants of OperatingSystem and ProgLang. -}
allProgrammers :: [Programmer]
allProgrammers = makeProgrammer <$> allOperatingSystems <*> allLanguages
  where makeProgrammer o l = Programmer { os = o, lang = l }
```

### The Quad

Determine how many unique inhabitants each type has.

```haskell
data Quad
    = One
    | Two
    | Three
    | Four
    deriving (Eq, Show)

-- 1
eQuad :: Either Quad Quad

-- 2
prodQuad :: (Quad, Quad)

-- 3
funcQuad :: Quad -> Quad

-- 4
prodTBool :: (Bool, Bool, Bool)

-- 5
gTwo :: Bool -> Bool -> Bool

-- 6
fTwo :: Bool -> Quad -> Quad
```

1. 4 + 4 = 8
2. 4 * 4 = 16
3. 4 ^ 4 = 256
4. 2 * 2 * 2 = 8
5. 2 ^ 2 ^ 2 = 16
6. (2 ^ 4 = 16) ^ 4 = 65,536

### Trees

```haskell
data BinaryTree a
    = Leaf
    | Node (BinaryTree a) a (BinaryTree a)
    deriving (Eq, Ord, Show)

-- 1. Write map for BinaryTree.
mapTree :: (a -> b) -> BinaryTree a -> BinaryTree b
mapTree _ Leaf = Leaf
mapTree f (Node left a right) =
    Node (mapTree f left) (f a) (mapTree f right)

-- 2. Write functions to convert BinaryTree values to lists.
preorder :: BinaryTree a -> [a]
preorder Leaf = []
preorder (Node left a right) =
    [a] ++ preorder left ++ preorder right

inorder :: BinaryTree a -> [a]
inorder Leaf = []
inorder (Node left a right) =
    inorder left ++ [a] ++ inorder right

postorder :: BinaryTree a -> [a]
postorder Leaf = []
postorder (Node left a right) =
    postorder left ++ postorder right ++ [a]

-- 3. Write foldr for BinaryTree.
foldTree :: (a -> b -> b) -> b -> BinaryTree a -> b
foldTree _ acc Leaf = acc
foldTree f acc (Node left val right) =
    foldTree f (f val (foldTree f acc right)) left
```

### Multiple choice

```haskell
data Weekday
    = Monday
    | Tuesday
    | Wednesday
    | Thursday
    | Friday

f Friday = "Miller Time"
```

1. a
2. c
3. b
4. c

### Ciphers

```haskell
import Data.Char (chr, ord)
import Data.List (foldl')

cycled :: String -> String -> String
cycled msg key = (snd . foldl' f (0, "")) msg
  where len = length key
        f (i, s) c
            | c == ' '  = (i, s ++ " ")
            | otherwise = (i + (mod 1 len), s ++ [key !! (mod i len)])

shift :: Char -> Char -> Char
shift o c
    | c == ' '  = o
    | otherwise = chr $ (mod (ord o - ord 'A' + diff) 26) + ord 'A'
  where diff = ord c - ord 'A'

cipher :: String -> String -> String
cipher msg key = zipWith shift msg $ cycled msg key
```

### As-patterns

```haskell
import Data.Char (toUpper)
import Data.List (words)

{- 1. This should return True if (and only if) all the values in the first list
appear in the second list, though they need not be contiguous. -}
isSubseqOf :: (Eq a) => [a] -> [a] -> Bool
isSubseqOf []       _ = True
isSubseqOf a@(x:xs) b
    | elem x b  = isSubseqOf xs b
    | otherwise = False

{- 2. Split a sentence into words, then tuple each one with its capitalized
form. -}
capitalizeWords :: String -> [(String, String)]
capitalizeWords = map (\str@(x:xs) -> (str, toUpper x : xs)) . words
```

### Language exercises

```haskell
import Data.Char (toUpper)

-- 1. Write a function that capitalizes a word.
capitalizeWord :: String -> String
capitalizeWord (x:xs) = toUpper x : xs

{- 2. Write a function that capitalizes sentences in a paragraph. Recognize when a new sentence has begun by checking for periods. Reuse the capitalizeWord
function. -}
capitalizeParagraph :: String -> String
capitalizeParagraph (x:xs)= checkStart $ words $ toUpper x : xs
  where checkStart (x:[])   = x
        checkStart (x:y:xs)
            | elem '.' x = x ++ " " ++ checkStart (capitalizeWord y : xs)
            | otherwise  = x ++ " " ++ checkStart (y : xs)
```

### Phone exercise

```haskell
-- TODO
```

### Hutton's Razor

```haskell
data Expr
    = Lit Integer
    | Add Expr Expr

-- 1. Write the eval function, that reduces an expression to a final sum.
eval :: Expr -> Integer
eval (Lit i)   = i
eval (Add x y) = eval x + eval y

-- 2: Write a printer for the expressions.
printExpr :: Expr -> String
printExpr (Lit i)   = show i
printExpr (Add x y) = unwords [printExpr x, "+", printExpr y]
```
