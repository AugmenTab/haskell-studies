# Chapter 05: Types

### Type matching: Match the function to its type signature.

Presented as `function ⇒ type signature`:

1. a &rArr; c
2. b &rArr; d
3. c &rArr; b
4. d &rArr; a
5. e &rArr; e

### Type arguments: Given a function and its type, tell us what type results from applying some or all of the arguments.

```haskell
u = undefined
f :: a -> a -> a -> a; f = u
x :: Char; x = u
```

1. If the type of `f` is `a -> a -> a -> a`, and the type of `x` is `Char`, then the type of `f x` is:
    * a
2. If the type of `g` is `a -> b -> c -> b`, then the type of `g 0 'c' "woot"` is:
    * d
3. If the type of `h` is `(Num a, Num b) => a -> b -> b`, then the of `h 1.0 2` is:
    * d
4. If the type of `h` is `(Num a, Num b) => a -> b -> b`, then the type of `h 1 (5.5 :: Double)` is:
    * c
5. If the type of `jackal` is `(Ord a, Eq b) => a -> b -> a`, then the type of `jackal "keyboard" "has the word jackal in it"` is:
    * a
6. If the type of `jackal` is `(Ord a, Eq b) => a -> b -> a`, then the type of `jackal "keyboard"` is:
    * e
7. If the type of `kessel` is `(Ord a, Num b) => a -> b -> a`, then the type of `kessel 1 2` is:
    * d
8. If the type of `kessel` is `(Ord a, Num b) => a -> b -> a`, then the type of `kessel 1 (2 :: Integer)` is:
    * a
9. If the type of `kessel` is `(Ord a, Num b) => a -> b -> a`, then the type of `kessel (1 :: Integer) 2` is:
    * c

### Apply yourself: Look at the provided pairs of functions. One function is unapplied, so the compiler will infer a maximally polymorphic type. The second function has been applied to a value, so the inferred type signature may have become concrete, or at least less polymorphic. Figure out how the type would change.

1. `(++) :: [a] -> [a] -> [a]` &rArr; `myConcat x = x ++ " yo"`
    * `(++) :: [Char] -> [Char]`
2. `(*) :: Num a => a -> a -> a` &rArr; `myMult x = (x / 3) * 5`
    * `(*) :: Num a => a -> a`
3. `take :: Int -> [a] -> [a]` &rArr; `myTake x = take x "hey you"`
    * `take :: Int -> [Char]`
4. `(>) :: Ord a => a -> a -> Bool` &rArr; `myCom x = x > (length [1..10])`
    * `(>) :: Num a => a -> Bool`
5. `(<) :: Ord a => a -> a -> Bool` &rArr; `myAlph x = x < 'z'`
    * `(<) :: Char -> Bool`

### Multiple choice

Presented as `question: multiple choice answer`

1. A value of type `[a]` is: c
2. A function of type `[[a]] -> [a]` could: a
3. A function of type `[a] -> Int -> a`: b
4. A function of type `(a, b) -> a`: c

### Determine the type: For the following functions, determine the type of the specified value.

1. All function applications return a value. Determine the values returned by these function applications and the types of those values:
    1. `(* 9) 6` &rArr; `54 :: Num a => a`
    2. `head [(0,"doge"),(1,"kitteh")]` &rArr; `(0,"doge") :: Num a => (a, [Char])`
    3. `head [(0 :: Integer ,"doge"),(1,"kitteh")]` &rArr; `(0,"doge") :: (Integer, [Char])`
    4. `if False then True else False` &rArr; `False :: Bool`
    5. `length [1, 2, 3, 4, 5]` &rArr; `5 :: Int`
    6. `(length [1, 2, 3, 4]) > (length "TACOCAT")` &rArr; `False :: Bool`
2. Given `x = 5; y = x + 5; w = y * 10`, what is the type of `w`?
    * `Num a => a`
3. Given `x = 5; y = x + 5; z y = y * 10`, what is the type of `z`?
    * `Num a => a -> a`
4. Given `x = 5; y = x + 5; f = 4 / y`, what is the type of `f`?
    * `Fractional a => a`
5. Given `x = "Julie"; y = "<3"; z = "Haskell"; f = x ++ y ++ z`, what is the type of `f`?
    * `[Char]`

### For each set of expressions, figure out which expression, if any, causes the compiler to squawk at you and why. Fix them if you can:

```haskell
-- 1
bigNum = (^) 5 $ 10
wahoo = bigNum $ 10


-- 2
x = print
y = print "woohoo!"
z = x "hello world!"


-- 3
a = (+)
b = 5
c = b 10
d = c 200


-- 4
a = 12 + b
b = 10000 * c
```

1. `wahoo = bigNum $ 10` will cause a compile error, because `bigNum` takes no arguments.
2. Nothing causes the compiler to crash. `x` is curried and waits until `print` receives its required parameter to evaluate.
3. The compile error occurs on `c = b 10` and `d = c 200`. `c` takes no arguments so it cannot be passed a value, and `d` has `c` in it which will be undefined. Since `a` is the prefix operator `(+)`, it is presumed that these values are all supposed to be added together, so they could be fixed like so: `c = a b 10; d = a c 200`.
4. `b = 10000 * c` causes the compiler to crash, because `c` is undefined.

### Type variable or specific type constructor?: You will be shown a type declaration, and you should categorize each type. The choices are: a fully polymorphic type variable, a constrained polymorphic type variable, or a concrete type constructor.

1. `f :: Num a => a -> b -> Int -> Int`
    * `a` is constrained
    * `b` is fully polymorphic
    * `Int` is concrete
2. `f :: zed -> Zed -> Blah`
    * `zed` is fully polymorphic
    * `Zed` and `Blah` are concrete
3. `f :: Enum b => a -> b -> C`
    * `a` is fully polymorphic
    * `b` is constrained
    * `C` is concrete
4. `f :: f -> g -> C`
    * `f` and `g` are fully polymorphic
    * `C` is constrained

### Write a type signature: For the following expressions, please add a type signature.

```haskell
-- 1
functionH (x:_) = x


-- 2
functionC x y =
  if (x > y) then True else False


-- 3
functionS (x, y) = y
```

1. `[a] -> a`
2. `functionC :: Ord a => a -> a -> Bool`
3. `functionS :: (a, b) -> b`

### Given a type, write the function: You will be shown a type and a function that needs to be written. Use the information the type provides to determine what the function should do.

1. `i :: a -> a`
    * `i a = a`
2. `c :: a -> b -> a`
    * `c a _ = a`
3. `c'' :: b -> a -> b` -- Given alpha equivalence, are the variables `c''` and `c` from the previous question the same thing?
    * `c'' b _ = b` -- Yes, they are the same. Both are still taking in two variables and returning only the first.
4. `c' :: a -> b -> b`
    * `c' _ b = b`
5. `r :: [a] -> [a]`
    * `r a = tail a`
6. `co :: (b -> c) -> (a -> b) -> a -> c`
    * `co bToC aToB a = bToC $ aToB a`
7. `a :: (a -> c) -> a -> a`
    * `a = _ a = a`
8. `a' :: (a -> b) -> a -> b`
    * `a' aToB a = aToB a`

### Fix it 1: Fix the broken code.

```haskell
-- Broken
module sing where


fstString :: [Char] ++ [Char]
fstString x = x ++ " in the rain"


sndString :: [Char] -> Char
sndString x = x ++ " over the rainbow"


sing = if (x > y) then
         fstString x or sndString y
where x = "Singin"
      x = "Somewhere"
```

```haskell
-- Fixed
module Sing where


fstString :: [Char] -> [Char]
fstString x = x ++ " in the rain"


sndString :: [Char] -> [Char]
sndString x = x ++ " over the rainbow"


sing = if (x > y) 
         then fstString x
         else sndString y
         where x = "Singin"
               y = "Somewhere"
```

### Fix it 2: Make a minor change so that it sings the other song.

```haskell
module Sing where


fstString :: [Char] -> [Char]
fstString x = x ++ " in the rain"


sndString :: [Char] -> Char
sndString x = x ++ " over the rainbow"


sing = if (x < y) 
         then fstString x
         else sndString y
         where x = "Singin"
               y = "Somewhere"
```

### Fix it 3

```haskell
-- arith3broken.hs
module Arith3Broken where


main :: IO ()
Main = do
  print 1 + 2
  putStrLn 10
  print (negate -1)
  print ((+) 0 blah)
  where blah = negate 1
```

```haskell
-- arith3fixed.hs
module Arith3Fixed where


main :: IO ()
main =
  do print (1 + 2)
     putStrLn $ show 10
     print (negate (-1))
     print ((+) 0 blah)
       where blah = negate 1
```

### Type-Kwon-Do: Repair the `???` declaration so that it passes the type checker.

```haskell
-- 1
f :: Int -> String
f = undefined


g :: String -> Char
g = undefined


h :: Int -> Char
h = ???


-- 2
data A
data B
data C


q :: A -> B
q = undefined


w :: B -> C
w = undefined


e :: A -> C
e = ???


-- 3
data X
data Y
data Z


xz :: X -> Z
xz = undefined


yz :: Y -> Z
yz = undefined


xform :: (X, Y) -> (Z, Z)
xform = ???


-- 4
munge :: (x -> y)
      -> (y -> (w, z))
      -> x
      -> w
munge = ???
```

1. `h x = g $ f x`
2. `e x = w $ q x`
3. `xform (x, y) = (,) (xz x) (yz y)`
4. `munge xToY yToWZ x = fst $ yToWZ $ xToY x`
