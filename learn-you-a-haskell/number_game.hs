--number_game
import Control.Monad (when)
import System.Random


main :: IO ()
main = do
    gen <- getStdGen
    askForNumber gen


askForNumber :: StdGen -> IO ()
askForNumber gen = do
    let (randNumber, newGen) = randomR (1, 10) gen :: (Int, StdGen)
    putStrLn "\nGuess a number between 1 and 10."
    numberString <- getLine
    when (not $ null numberString) $ do
        let number = read numberString :: Int
        if randNumber == number
            then putStrLn "You are correct!"
            else putStrLn $ "Sorry, it was " ++ show randNumber ++ "."
        askForNumber newGen


{-
main :: IO ()
main = do
    gen <- getStdGen
    let (randNumber, _) = randomR (1, 10) gen :: (Int, StdGen)
    putStrLn "\nGuess a number between 1 and 10."
    numberString <- getLine
    when (not $ null numberString) $ do
        let number = read numberString :: Int
        if randNumber == number
            then putStrLn "You are correct!"
            else putStrLn "Sorry, it was " ++ show randNumber ++ "."
        newStdGen
        main
-}
