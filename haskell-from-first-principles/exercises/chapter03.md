# Chapter 03:

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

1. [reverse.hs](../test-files/reverse.hs)
