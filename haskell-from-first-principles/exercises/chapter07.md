# Chapter 07:

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
