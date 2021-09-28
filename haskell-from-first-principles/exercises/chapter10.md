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
