# Chapter 07: More Functional Patterns

### Grab bag

1. Which (two or more) of the following are equivalent?
   * They are all equivalent.
2. The type of `mTh` (above) is `Num a => a -> a -> a -> a`. Which is the type of `mTh 3`?
   * a
3.  1. Rewrite the `f` function in the `where` clause:
        * `where f = \n -> n + 1`
    2. Rewrite the following to use anonymous lambda syntax: `addFive x y = (if x > y then y else x) + 5`
        * `addFive = \x -> \y -> (if x > y then y else x) + 5`
    3. Rewrite the following so that it doesn't use anonymous lambda syntax: `mflip f = \x -> \y -> f y x`
        * `mflip f y x = f y x`

### Variety pack

```haskell
-- 1
k (x, y) = x
k1 = k ((4-1), 10)
k2 = k ("three", (1 + 2))
k3 = k (3, True)

-- 2
f :: (a, b, c)
  -> (d, e, f)
  -> ((a, d), (c, f))
f = undefined
```

1. Given the above declarations:
    1. What is the type of `k`?
        * `(a, b) -> a`
    2. What is the type of `k2`? Is it the same as `k1` or `k3`?
        * `[Char]`. Not the same type.
    3. Of `k1`, `k2`, `k3`, which will return the number 3 as the result?
        * `k1`, `k3`
2. Fill in the definition of `f`:
   * `f (a, _, c) (d, _, f) = ((a, d),(c, f))`

### Case practice

```haskell
-- 1: Rewrite as a case expression
functionC x y = if (x > y) then x else y

functionC' x y =
  case x > y of
    True  -> x
    False -> y
-- 2: Rewrite as a case expression
ifEvenAdd2 n = if even n then (n+2) else n

ifEvenAdd2' n = 
  case even n of
    True  -> n + 2
    False -> n
-- 3: This case expression doesn't have all cases covered - fix it.
nums x =
  case compare x 0 of
    LT -> -1
    GT -> 1
    EQ -> 0  -- This line is the correction made.
```

### Artful dodgy: Determine what the following expressions reduce to.

Types added as suggested in the exercise.

```haskell
dodgy :: Num a => a -> a -> a
dodgy x y = x + y * 10

oneIsOne :: Num a => a -> a
oneIsOne = dodgy 1

oneIsTwo :: Num a => a -> a
oneIsTwo = (flip dodgy) 2
```

1. `dodgy 1 0`
   * 1
2. `dodgy 1 1`
   * 11
3. `dodgy 2 2`
   * 22
4. `dodgy 1 2`
   * 21
5. `dodgy 2 1`
   * 12
6. `oneIsOne 1`
   * 11
7. `oneIsOne 2`
   * 21
8. `oneIsTwo 1`
   * 21
9. `oneIsTwo 2`
   * 22
10. `oneIsOne 3`
    * 31
11. `oneIsTwo 3`
    * 23

### Guard duty

```haskell
-- 1, 2
avgGrade :: (Fractional a, Ord a) => a -> Char
avgGrade x
  | y >= 0.9  = 'A'
  | y >= 0.8  = 'B'
  | y >= 0.7  = 'C'
  | y >= 0.59 = 'D'
  | y <  0.59 = 'F'
  where y = x / 100

-- 3, 4, 5
pal xs
  | xs == reverse xs = True
  | otherwise        = False

-- 6, 7, 8
numbers x
  | x < 0  = -1
  | x == 0 = 0
  | x > 0  = 1
```

1. What happens if you put `otherwise = 'F'` as the top-most guard, then pass arguments of 90, 75, and 60?
   * The result of any arguments passed to the function will always be `'F'`.
2. What happens if you move `| y >= 0.7 = 'C'` as the top-most guard, then pass the argument 90, which should be an `'A'`?
   * The result of any arguments greater than or equal to 70 will be `'C'`, since that is the first guard.
3. What does the function return?
   * b
4. What types of arguments can `pal` take?
   * `Eq a => [a]`
5. What is the type of the function `pal`?
   * `pal :: Eq a => [a] -> Bool`
6. What does the function return?
   * c
7. What types of arguments can `numbers` take?
   * `(Ord a, Num a) => a`
8. What is the type of the function `numbers`?
   * `numbers :: (Ord a, Num a, Num p) => a -> p`

### Multiple choice

1. A polymorphic function:
   * d
2. Two functions named `f` and `g` have types `Char -> String` and `String -> [String]`, respectively. The composed function `g . f` has the type:
   * b
3. A function `f` has the type `Ord a => a -> a -> Bool`, and we apply to it one numeric value. What is the type now?
   * d
4. A function with the type `(a -> b) -> c`:
   * b
5. Given the following definition of `f`, what is the type of `f True`? `f x = x :: a -> a`
   * a


### Let's write code

[chapterseven.hs](../test-files/chapterseven.hs)