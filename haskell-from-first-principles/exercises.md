# Exercises

The following are my answers and solutions to the questions and problems posed in the chapter exercises.

## Chapter 01:

### Equivalence: We'll give you a lambda expression. Keeping in mind both alpha equivalence and how multiple heads are nested, choose an answer that is equivalent to the listed lambda term:

1. *&lambda;xy.xz*
   * b: *&lambda;mn.mz*
2. *&lambda;xy.xxy*
   * c: *&lambda;a.(&lambda;b.aab)*
3. *&lambda;xyz.zx*
   * b: *&lambda;tos.st*

### Combinators: Determine whether each of the following functions are combinators or not:

1. *&lambda;x.xxx*
   * yes
2. *&lambda;xy.zx*
   * no
3. *&lambda;xyz.xy(zx)*
   * yes
4. *&lambda;xyz.xy(zxy)*
   * yes
5. *&lambda;xy.xy(zxy)*
   * no

### Normal form or diverge?: Determine whether each of the following expressions can be reduced to a normal form or if they diverge:

1. *&lambda;x.xxx*
   * normal form
2. *(&lambda;z.zz)(&lambda;y.yy)*
   * diverges: *(&lambda;z.zz)(&lambda;y.yy) &rArr; (&lambda;y.yy)(&lambda;y.yy) ...*
3. *(&lambda;x.xxx)z*
   * normal form: *(&lambda;x.xxx)z &rArr; zzz* 

### Beta reduce: Evaluate (that is, beta reduce) each of the following expressions to normal form.

1. *(&lambda;abc.cba)zz(&lambda;wv.w)*
   * &rArr; *z*
2. *(&lambda;x.&lambda;y.xyy)(&lambda;a.a)b*
   * &rArr; *bb*
3. *(&lambda;y.y)(&lambda;x.xx)(&lambda;z.zq)*
   * &rArr; *qq*
4. *(&lambda;z.z)(&lambda;z.zz)(&lambda;z.zy)*
   * &rArr; *yy*
5. *(&lambda;x.&lambda;y.xyy)(&lambda;y.y)y*
   * &rArr; *yy*
6. *(&lambda;a.aa)(&lambda;b.ba)c*
   * &rArr; *aac*
7. *(&lambda;xyz.xz(yz))(&lambda;x.z)(&lambda;x.a)*
   * &rArr; *&lambda;z1.za*

## Chapter 02:

### Heal the sick: The following code samples are broken and won't compile. The first two are as you might enter them into the REPL. The third is from a source file. Find the mistakes, and fix them so that the code will compile:

1. `area x = 3. 14 * (x * x)`
   * `area x = 3.14 * (x * x)` -- Removed space from `3. 14`
2. `double x = b * 2`
   * `double x = x * 2` -- Variable `b` is not defined. Renamed to `x`.
3. Code snippet below, followed by correction: indentation is not consistent.

```haskell
-- Original
x = 7
 y = 10
f = x + y

-- Corrected
x = 7
y = 10
f = x + y
```

### A head code: Determine in your head what the following expressions will return. These examples are prefixed with `let`, because they are not declarations, they are expressions:

1. `let x = 5 in x`
   * 5
2. `let x = 5 in x * x`
   * 25
3. `let x = 5; y = 6 in x * y`
   * 30
4. `let x = 3; y = 1000 in x + 3`
   * 6
    
### Rewrite these `let` expressions with `where` clauses:

1. `let x = 3; y = 1000 in x * 3 + y`
2. `let y = 10, x = 10 * 5 + y in x * 5`
3. `let x = 7; y = negate x; z = y * 10 in z / x + y`

```haskell
rewrite1  = x * 3 + y
  where x = 3
        y = 1000


rewrite2  = x * 5
  where y = 10
        x = 10 * 5 + y


rewrite3  = z / x + y
  where x = 7
        y = negate x
        z = y * 10
```

### Parenthesization: Parenthesize the following expressions more explicitly without changing their results:

1. `2 + 2 * 3 - 1`
   * `2 + (2 * 3) - 1`
2. `(^) 10 $ 1 + 1`
   * `(^) 10 (1 + 1)`
3. `2 ^ 2 * 4 ^ 5 + 1`
   * `((2 ^ 2) * (4 ^ 5)) + 1`

### Equivalent expressions: Which of the following pairs of expressions will return the same result when evaluated?

1. `1 + 1`, `2`
2. `10 ^ 2`, `10 + 9 * 10`
3. `400 - 37`, `(-) 37 400`
4. ```100 `div` 3```, `100 / 3`
5. `2 * 5 + 18`, `2 * (5 + 18)`

* Answer: 1, 2

### More fun with functions: Given a bit of code as it might be entered into a source file, rewrite it such that it could be evaluated in the REPL:

```haskell
z = 7


x = y ^ 2


waxOn = x * 5


y = z + 8
```

* Answer: `waxOn = let z = 7; y = z + 8; x = y ^ 2 in x * 5`

### Rewrite `waxOn` as an expression with a `where` clause:

```haskell
waxOn     = x * 5
  where z = 7
        y = z + 8
        x = y ^ 2
```

## Chapter 03:

### Syntax errors: Read the syntax of the following functions, and decide whether it will compile:

1. `++ [1, 2, 3] [4, 5, 6]`
   * no
2. `'<3' ++ ' Haskell'`
   * no
3. `concat ["<3", " Haskell"]`
   * yes

### Reading syntax 1: For the following lines of code, read the syntax carefully, and decide whether they are written correctly. Correct as many as you can.

1. `concat [[1, 2, 3], [4, 5, 6]]`
   * Correct.
2. `++ [1, 2, 3] [4, 5, 6]`
   * Incorrect. Fixed: `(++) [1, 2, 3] [4, 5, 6]`
3. `(++) "hello" " world"`
   * Correct.
4. `["hello" ++ " world"]`
   * Correct.
5. `4 !! "hello"`
   * Incorrect. Fixed: `"hello" !! 4`
6. `(!!) "hello" 4`
   * Correct.
7. `take "4 lovely"`
   * Incorrect. Fixed: `take 4 "lovely"`
8. `take 3 "awesome"`
   * Correct.

### Reading syntax 2: We have two sets. The first set is lines of code, and the other is a set of results. Read the code, and figure out which results come from which lines of code.

Presented as `code ⇒ output`:

1. a &rArr; d
2. b &rArr; c
3. c &rArr; e
4. d &rArr; a
5. e &rArr; b

### Building functions 1: Write functions that take the given input and return the given outputs.

Presented as `given ⇒ return`.`:

1. "Curry is awesome" &rArr; "Curry is awesome!"
   * `(++) "Curry is awesome" "!"`
2. "Curry is awesome!" &rArr; "y"
   * `[(!!) "Curry is awesome!" 4]` -- Must be wrapped in square brackets, or else it returns a `Char` instead of a `String`.
3. "Curry is awesome!" &rArr; "awesome!"
   * `drop 9 "Curry is awesome!`

### Building functions 2: Take each of the above, and rewrite it as a general function that could take different `String` inputs as arguments but retain the same behavior. Use a variable as an argument to each of your (named) functions.

```haskell
exclaim :: String -> String
exclaim s = (++) s "!"


returnFifthLetter :: String -> String
returnFifthLetter s = [(!!) s 4]


stripFirst9 :: String -> String
stripFirst9 s = drop 9 s
```

### Building functions 3: Write a function of type `String -> Char` that returns the third character in a `String`.  Give the function a name, then apply it to a variable, not a specific `String`, so that it can be reused for different `String` inputs.

```haskell
thirdLetter :: String -> Char
thirdLetter x = (!!) x 2
```

### Building functions 4: Change the above function so that the `String` operated on is always the same, and the variable represents the number of the letter you want to return.

```haskell
letterIndex :: Integer -> Char
letterIndex x = "Curry is awesome!" !! (x - 1) -- or (!!) x ((-1) n)
-- remove '-1' if specific index instead of nth letter in string.
```

### Building functions 5: Using the `take` and `drop` functions, write a function called `rvrs` (an abbreviation of "reverse"), which takes the String "Curry is awesome" and returns the result "awesome is Curry".

```haskell
rvrs :: String -> String
rvrs x = concat [drop 9 x, take 4 (drop 5 x), take 5 x]


-- "Try using $ to avoid using parentheses."
rvrsNoParens :: String -> String
rvrsNoParens x = concat [drop 9 x, take 4 $ drop 5 x, take 5 x]
```

### Building functions 6: Expand the function above into a module.

1. [reverse.hs](/haskell-from-first-principles/test-files/reverse.hs)

## Chapter 04:

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
