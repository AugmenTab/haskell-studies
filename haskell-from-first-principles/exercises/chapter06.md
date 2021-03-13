# Chapter 06

### `Eq` instances: Write the `Eq` instances for the datatypes provided.

[typeclasses.hs](../test-files/typeclasses.hs)

### Will they work?: Given the following code examples, decide if they will work, what result they will return if they do, and why (or why not) they work.

1. `max (length [1, 2, 3]) (length [8, 9, 10, 11, 12])`
    * This will work, because `length` returns in each case an `Int`, which has an instance of `Ord`. It will return 5, which is the value of `length [8, 9, 10, 11, 12]`.
2. `compare (3 * 4) (3 * 5)`
    * This will work, because the `*` operator returns in each case a `Num`, which has an instance of `Ord`. It will return LT, because `(3 * 4`) is less than `(3 * 5)`.
3. `compare "Julie" True`
    * This will not work, because although `"Julie"` and `True` both have an instance of `Ord`, they are not the same type.
4. `(5 + 3) > (3 + 6)`
    * This will work, because the `+` operator returns in each case a `Num`, which has an instance of `Ord`. It will return False, because `(5 + 3)` is not greater than `(3 + 6)`.

### Multiple choice

1. c
2. b
3. a
4. c
5. a

### Does it type check? Examine the following code, and decide whether it will type check. If you can, fix the error.

[typecheck.hs](../test-files/typecheck.hs)

1. No, because `Person` does not derive `Show`. It is fixed in the file.
2. No, because there is no instance for `Eq` for Mood. I have added one in order to correct it for question 3.
3.
    1. `settleDown` can only accept a `Mood`, which has a value of either `Woot` or `Blah`.
    2. It will not work, because none of the values of `Mood` are of any `Num` types.
    3. It will not work, because there is no `Ord` in the `Mood` definition. I have fixed this in the file. Now that `Mood` derives `Ord`, it returns False, because `Blah` comes before `Woot` in the definition of `Mood`.
4. Yes, this type checks.

### Given a datatype declaration, what can we do?: Given the following datatype definitions, which of the following will type check? For those that don't, why don't they?

```haskell
data Rocks =
  Rocks String deriving (Eq, Show)

data Yeah =
  Yeah Bool deriving (Eq, Show)

data Papu =
  Papu Rocks Yeah
  deriving (Eq, Show)
```

```haskell
-- 1
phew = Papu "chases" True

-- 2
truth = Papu (Rocks "chomskydoz")
             (Yeah True)

-- 3
equalityForAll :: Papu -> Papu -> Bool
equalityForAll p p' = p == p'

-- 4
comparePapus :: Papu -> Papu -> Bool
comparePapus p p' = p > p'
```

1. This will not type check. Papu expects an instance of Rocks and Yeah, but is instead receiving a String and a Bool.
2. This will type check.
3. This will type check.
4. This will not type check; Papu does not derive `Ord`.

### Match the types: Given two types, can you substitute the second type for the first?

```haskell
-- 1
i :: Num a => a
i = 1
  
-- 2, 3, 4
f :: Float
f = 1.0

-- 5
freud :: a -> a
freud x = x

-- 6
freud' :: a -> a
freud' x = x
  
-- 7
myX = 1 :: Int

sigmund :: Int -> Int
sigmund x = myX

-- 8
myX = 1 :: Int

sigmund' :: Int -> Int
sigmund' x = myX

-- 9
jung :: Ord a => [a] -> a
jung xs = head (sort xs)

-- 10
young :: [Char] -> Char
young xs = head (sort xs)

-- 11
mySort :: [Char] -> Char
mySort = sort

signifier :: [Char] -> Char
signifier xs = head (mySort xs)
```

1. `i :: a`
    * No, this can't be substituted. It requires an instance of `Num`.
2. `f :: Num a => a`
    * No, this won't work. It requires some instance of `Fractional`.
3. `f :: Fractional a => a`
    * Yes, this will work.
4. `f :: RealFrac a => a`
    * Yes, this will work.
5. `freud :: Ord a => a -> a`
    * Yes, this will work.
6. `freud' :: Int -> Int`
    * Yes, this will work.
7. `sigmund :: a -> a`
    * No, this will not work. It requires an instance of `Num`.
8. `sigmund' :: Num a => a -> a`
    * No, this will not work. `myX` is already of type Int, so `sigmund'` cannot be made more polymorphic.
9. `jung :: [Int] -> Int`
    * Yes, this will work.
10. `young :: Ord a => [a] -> a`
    * Yes, this will work.
11. `signifier :: Ord a => [a] -> a`
    * No, this will not work. `mySort` is already of type `[Char] -> Char`, so `signifier` cannot be made more polymorphic.

### Type-Kwon-Do Two: Electric typealoo: Fill in the `???` with code that will fit the types.

```haskell
-- 1
chk :: Eq b => (a -> b) -> a -> b -> Bool
chk = ???

-- 2
arith :: Num b
         => (a -> b)
         -> Integer
         -> a
         -> b
arith = ???         
```

1. `chk f x y = f x == y`
2. `arith f x y = (f y) + (fromIntegral x)`
