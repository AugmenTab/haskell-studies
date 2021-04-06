# Chapter 04: Basic Datatypes

### Mood swing: Given this datatype, answer the following questions:

`data Mood = Blah | Woot deriving Show`

1. What is the type constructor, or name of this type?
    * Mood
2. If the function requires a Mood value, what are the values you could possibly use?
    * Blah, Woot
3. We are trying to write a function `changeMood` to change Chris's mood instantaneously. It should act like `not` in that, given one value, it returns the *other* value of the same type. So far, we've written a type signature `changeMood :: Mood -> Wort.` What's wrong with that?
    * changeMood could return either Blah or Woot, depending on which was given to it. The type signature should read `changeMood :: Mood -> Mood`.
4. Now we want to write the function that changes his mood. Given an input mood, it gives us the other one. Fix any mistakes and complete the function:

```haskell
-- Original
changeMood Mood = Woot
changeMood    _ = Blah


-- Fixed
changeMood Mood =
  case Mood of Woot -> Blah
               Blah -> Woot
```

### Find the mistakes: The following lines of code may have mistakes - some of them won't compile! You know what you need to do:

1. `not True && true`
    * Will not compile. `true` must be capitalized to `True`. Corrected: `not True && True`
2. `not (x = 6)`
    * Will not compile. Single `=` is assignment, so double equal `==` is needed for comparison. Corrected: `not (x == 6)`
3. `(1 * 2) > 5`
    * OK
4. `[Merry] > [Happy]`
    * Will not compile. Merry and Happy aren't types, so it is assumed they are intended to be instances of `String`. Corrected: `["Merry"] > ["Happy"]`
5. `[1, 2, 3] ++ "look at me!"`
    * Will not compile. Cannot concatenate a `String` to a list of numbers. Corrected: `(concat (map show [1, 2, 3])) ++ " look at me!"`

### Lists: Answer the questions, keeping the following code in mind:

```haskell
awesome = ["Papuchon", "curry", ":)"]
also = ["Quake", "The Simons"]
allAwesome = [awesome, also]
```

1. Given the definition of `length` (a function that takes a list and returns the number of items in that list), what would its type signature be? How many arguments does it take and of what types? What is the type of the result it evaluates to?
    * `length :: Foldable t => t a -> Int`
2. What are the results of the following expressions:
    1. `length [1, 2, 3, 4, 5]`
        * 5
    2. `length [(1, 2), (2, 3), (3, 4)]`
        * 3
    3. `length allAwesome`
        * 2
    4. `length (concat allAwesome)`
        * 5
3. Consider the expressions `6 / 3` and `6 / length [1, 2, 3]`. One works, and one returns an error. Determine which will return an error and why.
    * `6 / length [1, 2, 3]` returns an error because the `/` operator expects a Fractional number type, and `length` returns an `Int`.
4. How can you fix the broken code from the preceding exercise using a different division function/operator?
    * By using `div`: `div 6 $ length [1, 2, 3]`
5. What is the type of the expression `2 + 3 == 5`? What would we expect as a result?
    * It is a `Bool`, and we would expect it to return `True`.
6. What is the type and expected result value of the following:
    1. `x = 5`
    2. `x + 3 == 5`
7. Below are some bits of code. Which will work? Why or why not? If they work, what values would these reduce to?
    1. `length allAwesome == 2`
        * It will work. It evaluates to True.
    2. `length [1, 'a', 3, 'b']`
        * It won't work, because you cannot have a list with mixed types.
    3. `length allAwesome + length awesome`
        * It will work, because the result of the `length` functions will result in compatible number types.
    4. `(8 == 8) && ('b' < 'a')`
        * It will work. It evaluates to False, because 'b' is not less than 'a'.
    5. `(8 == 8) && 9`
        * It won't work, because 9 cannot evaluate to a `Bool`.
8. Write a function that tells you whether or not a given `String` (or list) is a palindrome.
    * Implemented below.
9. Write a function to return the absolute value of a number using an if-then-else expression:
    * Implemented below.
10. Fill in the definition of the following function, using `fst` and `snd`:
    * Implemented below.

```haskell
-- #8
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome x = reverse x == x


-- #9
myAbs :: Integer -> Integer
myAbs x = if x < 0 then negate x else x


-- #10:
f :: (a, b) -> (c, d) -> ((b, d), (a, c))
f x y = ((snd x, snd y), (fst x, fst y))
```

### Correcting syntax: In the following examples, you'll be shown syntactically incorrect code. Type it in, and try to correct it in your text editor.

```haskell
-- 1: Here we want a function that adds 1 to the length of a string argument and returns that:
x = (+)

F xs = w 'x' 1
     where w = length xs


-- 2: This is supposed to be the identity function, id:
\X = x


-- 3: When fixed, this function will return 1 from the value (1, 2):
f (a b) = A
```

1. The variable `x` is being treated as a `Char`, and must come before the arguments since it is an infix operator. The function name `F` cannot start with a capital letter. Fixed:`f xs = x w 1 where w = length xs`
2. Function must have a name, and `x` in function body must match parameter. Fixed: `f x = x`
3. `A` in function body must match parameter, and tuple parameter must separate items with a comma. Fixed: `f (a, b) = a`

### Match the function names to their types

1. Which of the following types is the type of `show`?
    * c
2. Which of the following types is the type of `==`?
    * b
3. Which of the following types is the type of `fst`?
    * a
4. Which of the following types is the type of `+`?
    * d
