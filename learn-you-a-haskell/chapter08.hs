-- chapter08: Input and Output
module Chapter08 where

import Control.Monad
-- import Data.Char


{-
main :: IO ()
main = do
    putStrLn "Hello! What's your name?"
    name <- getLine
    putStrLn $ "Hey " ++ name ++ ", you rock!"    
-}

{-
main :: IO ()
main = do
    putStrLn "What's your first name?"
    firstName <- getLine
    putStrLn "What's your last name?"
    lastName <- getLine
    let bigFirstName = map toUpper firstName
        bigLastName  = map toUpper lastName
    putStrLn $ "Hey " ++ bigFirstName ++ " "
                      ++ bigLastName
                      ++ ", how are you?"
-}

{-
main :: IO ()
main = do
    input <- getLine
    when (input == "SWORDFISH") $ do
        putStrLn input
-}

{-
main :: IO ()
main = do
    rs <- sequence [getLine, getLine, getLine]
    print rs
    mapM_ print rs
-}

{-
main :: IO ()
main = forever $ do
    putStrLn "Give me some input:"
    l <- getLine
    putStrLn $ map toUpper l
-}

main :: IO [()]
main = do
    colors <- forM [1..4] (\a -> do
        putStrLn $ "Which color do you associate with the number " 
                   ++ show a ++ "?"
        color <- getLine
        return color)
    putStrLn "The colors that you associate with 1, 2, 3, and 4 are: "
    mapM putStrLn colors
