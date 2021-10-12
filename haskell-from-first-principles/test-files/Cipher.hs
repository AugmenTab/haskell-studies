module Cipher where

import Data.Char (chr, ord, toLower)
import Data.List (foldl')

main :: IO ()
main = do
  putStr "Would you like to use the Caesar or Vigenère cipher?: "
  cmd <- getLine
  if null cmd
    then do
      putStrLn "You have failed to make a selection."
    
    else case toLower $ head cmd of
      'c' -> caesarEntry
      'v' -> vigenereEntry
      _   -> do
        putStrLn "Please choose between the Caesar and Vigenère ciphers."
        main

caesarEntry :: IO ()
caesarEntry = do
  putStr "Would you like to Encode or Decode a passage?: "
  cmd <- getLine
  if null cmd
    then do
      putStrLn "You have failed to make a selection."
      caesarEntry
    
    else case toLower $ head cmd of
      'e' -> do
        putStr "Enter the phrase to encode: "
        msg   <- getLine
        putStr "Enter the shift value: "
        shift <- getLine
        putStrLn $ caesar msg (read shift :: Int)

      'd' -> do
        putStr "Enter the phrase to decode: "
        msg   <- getLine
        putStr "Enter the shift value: "
        shift <- getLine
        putStrLn $ unCaesar msg (read shift :: Int)

      _   -> do
        putStrLn "Please choose between Encoding and Decoding a message."
        caesarEntry

vigenereEntry :: IO ()
vigenereEntry = do
  putStr "Enter the phrase to encode: "
  msg <- getLine
  putStr "Enter the key phrase: "
  key <- getLine

  case (null msg, null key) of
    (True, True) -> do
      putStrLn "Please enter the phrase to encode and the key phrase."
      vigenereEntry

    (True, _) -> do
      putStrLn "Please enter the phrase to encode."
      vigenereEntry
    
    (_, True) -> do
      putStrLn "Please enter the key phrase."
      vigenereEntry
    
    _ -> putStrLn $ vigenere msg key
    

-- Caesar
caesar :: String -> Int -> String
caesar []     _ = []
caesar (x:xs) n = (chr . (+97) $ mod ((ord x) - 97 + n) 26) : caesar xs n

unCaesar :: String -> Int -> String
unCaesar x n = caesar x (negate n)

-- Vigenère
vigenere :: String -> String -> String
vigenere msg key = zipWith shift msg $ cycled msg key

shift :: Char -> Char -> Char
shift o c
    | c == ' '  = o
    | otherwise = chr $ (mod (ord o - ord 'A' + diff) 26) + ord 'A'
  where diff = ord c - ord 'A'

cycled :: String -> String -> String
cycled msg key = (snd . foldl' f (0, "")) msg
  where len = length key
        f (i, s) c
            | c == ' '  = (i, s ++ " ")
            | otherwise = (i + (mod 1 len), s ++ [key !! (mod i len)])
