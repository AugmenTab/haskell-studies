module Main where

import Control.Monad (forever)
import Data.Char (toLower)
import Data.Maybe (isJust)
import Data.List (intersperse)
import System.Exit (exitSuccess)
import System.IO (BufferMode(NoBuffering), hSetBuffering, stdout)
import System.Random (randomRIO)

newtype WordList = WordList [String] deriving (Eq, Show)

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  word <- randomWord'
  let puzzle = freshPuzzle (fmap toLower word)
  runGame puzzle

allWords :: IO WordList
allWords = do
  dict <- readFile "data/dict.txt"
  pure $ WordList (lines dict)

minWordLength :: Int
minWordLength = 5

maxWordLength :: Int
maxWordLength = 9

gameWords :: IO WordList
gameWords = do
  (WordList aw) <- allWords
  pure $ WordList (filter gameLength aw)
  where gameLength w =
          let l = length (w :: String)
           in l >= minWordLength && l < maxWordLength

randomWord :: WordList -> IO String
randomWord (WordList wl) = do
  randomIndex <- randomRIO (0, length wl - 1)
  pure $ wl !! randomIndex

randomWord' :: IO String
randomWord' = gameWords >>= randomWord

data Puzzle = Puzzle String [Maybe Char] [Char] Int

instance Show Puzzle where
  show (Puzzle _ discovered guessed wrong) =
    (intersperse ' ' $ fmap renderPuzzleChar discovered)
    ++ "  ||  Guessed so far: " ++ guessed
    ++ "  ||  Wrong Guesses: "  ++ show wrong

freshPuzzle :: String -> Puzzle
freshPuzzle str = Puzzle str fresh [] 0
  where fresh = replicate (length str) Nothing

charInWord :: Puzzle -> Char -> Bool
charInWord (Puzzle str _ _ _) c = elem c str

alreadyGuessed :: Puzzle -> Char -> Bool
alreadyGuessed (Puzzle _ _ guessed _) c = elem c guessed

renderPuzzleChar :: Maybe Char -> Char
renderPuzzleChar mbChar = case mbChar of
  Just c  -> c
  Nothing -> '_'

fillInCharacter :: Puzzle -> Char -> Puzzle
fillInCharacter (Puzzle word filledInSoFar s wrong) c =
  Puzzle word newFilledInSoFar (c : s) wrong
  where zipper guessed wordChar guessChar = 
          if wordChar == guessed
            then Just wordChar
            else guessChar
        newFilledInSoFar = zipWith (zipper c) word filledInSoFar

handleGuess :: Puzzle -> Char -> IO Puzzle
handleGuess puzzle@(Puzzle w f g i) guess = do
  case (charInWord puzzle guess, alreadyGuessed puzzle guess) of
    (_, True) -> do
      putStrLn "You already guessed that character, pick something else!\n"
      pure puzzle

    (True, _) -> do
      putStrLn "This character was in the word.\
              \ Filling in the word accordingly.\n"
      pure (fillInCharacter puzzle guess)
    
    (False, _) -> do
      putStrLn "This character wasn't in the word, try again.\n"
      pure (fillInCharacter (Puzzle w f g (i + 1)) guess)

gameOver :: Puzzle -> IO ()
gameOver (Puzzle wordToGuess _ _ wrong) =
  if wrong == 6
    then do
      putStrLn "You lose!"
      putStrLn $ "The word was: " ++ wordToGuess
      exitSuccess
    else pure ()

gameWin :: Puzzle -> IO ()
gameWin (Puzzle wordToGuess filledInSoFar _ _) =
  if all isJust filledInSoFar
    then do
      putStrLn $ "You win! The word was: " ++ wordToGuess
      exitSuccess
    else pure ()

runGame :: Puzzle -> IO ()
runGame puzzle@(Puzzle _ _ _ wrong) = forever $ do
  gameOver puzzle
  gameWin puzzle
  putStrLn $ drawHangman wrong
  putStrLn $ "Current puzzle is: " ++ show puzzle
  putStr "Guess a letter: "
  guess <- getLine
  case guess of
    [c] -> handleGuess puzzle c >>= runGame
    _   -> putStrLn "Your guess must be a single character.\n"

drawHangman :: Int -> String
drawHangman 6 =
  " o---,  \n\
  \ |   O  \n\
  \ |  /|\\ \n\
  \ |   |  \n\
  \ |  / \\ \n\
  \/|\\"

drawHangman 5 =
  " o---,  \n\
  \ |   O  \n\
  \ |  /|\\ \n\
  \ |   |  \n\
  \ |  /   \n\
  \/|\\"

drawHangman 4 =
  " o---,  \n\
  \ |   O  \n\
  \ |  /|\\ \n\
  \ |   |  \n\
  \ |      \n\
  \/|\\"

drawHangman 3 =
  " o---,  \n\
  \ |   O  \n\
  \ |  /|  \n\
  \ |   |  \n\
  \ |      \n\
  \/|\\"

drawHangman 2 =
  " o---,  \n\
  \ |   O  \n\
  \ |   |  \n\
  \ |   |  \n\
  \ |      \n\
  \/|\\"

drawHangman 1 =
  " o---,  \n\
  \ |   O  \n\
  \ |      \n\
  \ |      \n\
  \ |      \n\
  \/|\\"

drawHangman _ =
  " o---,  \n\
  \ |      \n\
  \ |      \n\
  \ |      \n\
  \ |      \n\
  \/|\\"
  