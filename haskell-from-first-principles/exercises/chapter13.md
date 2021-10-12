# Chapter 13: Building Projects

### Intermission: Check your understanding

```haskell
-- blacktip
import qualified Control.Concurrent as CC
import qualified Control.Concurrent.MVar as MV
import qualified Data.ByteString.Char8 as B
import qualified Data.Locator as DL
import qualified Data.Time.Clock.POSIX as PSX
import qualified Filesystem as FS
import qualified Filesystem.Path.CurrentOS as FPC
import qualified Network.Info as NI

import qualified Safe
import           Control.Exception (mask, try)
import           Control.Monad (forever, when)
import           Data.Bits
import           Data.Bits.Bitwise (fromListBE)
import           Data.List.Split (chunksOf)
import           Database.Blacktip.Types
import           System.IO.Unsafe (unsafePerformIO)

writeTimestamp :: MV.MVar ServerState -> FPC.FilePath -> IO CC.ThreadId
writeTimestamp s path = do
    CC.forkIO go
    where go = forever $ do
        ss <- MV.readMVar s
        mask $ \_ -> do
            FS.writeFile path
            (B.pack (show (ssTime ss)))
        -- sleep for 1 second
        CC.threadDelay 1000000
```

1. What functions are being imported from `Control.Monad`?
    * `forever`, `when`
2. Which imports are both unqualified and imported in their entirety?
    * `Data.Bits`, `Database.Blacktip.Types`
3. From the name, what do you suppose importing the `Types` module brings in?
    * The project name is "Blacktip," so my guess would be that this is a module written by Chris to define types for the database.
4. Now, compare the `writeTimestamp` function to the import list.
    1. The type signature refers to three aliased imports. What modules are named in those aliases?
        * `MV` -> `Control.Concurrent.MVar`
        * `FPC` -> `Filesystem.Path.CurrentOS`
        * `CC` -> `Control.Concurrent`
    2. Which import does `FS.writeFile` refer to?
        * `Filesystem`
    3. Which import does `forever` come from?
        * `Control.Monad`

### Hangman game logic

Alter the Hangman game so that it ends after a number of incorrect guesses, and doesn't tell you that you lost if you guess the last letter on the last try.

[Hangman Game](../hangman/src/Main.hs)

### Modifying code

1. Open your ciphers module, and modify it so that the Caesar and Vigenère ciphers work with user input.
    * TODO

```haskell
{-
  2. Here is a very simple, short block of code. Notice it has a `forever` that
  will make it keep running, over and over again. Load it into your REPL, and
  test it out. Then, refer back to the chapter, and modify it to exit
  successfully after a `False` result.
-}
import Control.Monad (forever)
import Data.Char (isAlpha, toLower)
import System.Exit (exitSuccess)

palindrome :: IO ()
palindrome = forever $ do
  line1 <- getLine
  case (line1 == reverse line1) of
    True  -> putStrLn "It's a palindrome!"
    False -> putStrLn "Nope!"

palindrome' :: IO ()
palindrome' = forever $ do
  line1 <- getLine
  case (line1 == reverse line1) of
    True  -> putStrLn "It's a palindrome!"
    False -> do
      putStrLn "Nope!"
      exitSuccess

{-
  3. If you try using `palindrome` on a sentence such as "Madam I'm Adam," you
  may notice that it doesn't work. Modifying the above so that it works on
  sentences, too, involves several steps. You may need to refer back to previous
  examples in the chapter to get ideas for proper ordering and nesting. You may
  wish to import Data.Char to use the function `toLower`. Have fun.
-}
palindrome'' :: IO ()
palindrome'' = forever $ do
  sentence <- getLine
  let alphaOrAcc x acc =
        if isAlpha x
          then toLower x : acc
          else acc
      prepped = foldr alphaOrAcc [] sentence
   in case (prepped == reverse prepped) of
        True  -> putStrLn "It's a palindrome!"
        False -> do
          putStrLn "Nope!"
          exitSuccess

{-
  4. Your job is to write the function `gimmePerson`. Since `IO ()` is about the
  least informative type imaginable, we'll tell you what it should do:
    a) It should prompt the user for a name and age input.
    b) It should attempt to construct a `Person` value using the name and age
       the user enters. You'll need the `read` function for the age, because
       it's an `Integer` rather than a `String`.
    c) If it constructs a successful person, it should print `"Yay! Successfully
       got a person: "` followed by the `Person` value.
    d) If it gets an error value, it should report that an error occurred and
       print the error.
-}
type Name = String
type Age  = Integer

data Person = Person Name Age deriving Show

data PersonInvalid
  = NameEmpty
  | AgeTooLow
  | PersonInvalidUnknown String
  deriving (Eq, Show)

mkPerson :: Name -> Age -> Either PersonInvalid Person
mkPerson name age
  | name /= "" && age > 0 = Right $ Person name age
  | name == ""            = Left NameEmpty
  | not (age > 0)         = Left AgeTooLow
  | otherwise             =
      Left $ PersonInvalidUnknown $
        "Name was: " ++ show name ++
        " Age was: " ++ show age

gimmePerson :: IO ()
gimmePerson = do
  putStr "Enter your name: "
  name <- getLine
  putStr "Enter your age: "
  age  <- getLine
  case mkPerson name (read age :: Integer) of
    Right x        -> putStrLn $ "Yay! Successfully got a person: " ++ show x
    Left NameEmpty -> putStrLn "Error: you didn't enter a name."
    Left AgeTooLow -> putStrLn "Error: the age you entered was too low."
    Left (PersonInvalidUnknown e) -> putStrLn e
```
