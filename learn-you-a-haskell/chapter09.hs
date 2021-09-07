-- chapter09: More Input and More Output
module Chapter09 where

import Control.Exception
-- import Control.Monad
-- import Data.Char
-- import Data.List
-- import System.Environment
import System.IO
import System.Random
-- import qualified Data.ByteString.Lazy as B
-- import qualified Data.ByteString      as S


{-
main :: IO ()
main = forever $ do
    l <- getLine
    putStrLn $ map toUpper l
-}

{-
main :: IO ()
main = do
    contents <- getContents
    putStrLn $ map toUpper contents
-}

{-
main :: IO ()
main = do
    contents <- getContents
    putStrLn $ shortLinesOnly contents
-}

{-
main :: IO ()
main = interact shortLinesOnly
-}

{-
main :: IO ()
main = interact respondPalindromes
-}

{-
main :: IO ()
main = do
    handle <- openFile "io-files/girlfriend.txt" ReadMode
    contents <- hGetContents handle
    putStrLn contents
    hClose handle
-}

{-
main :: IO ()
main = do
    withFile "io-files/girlfriend.txt" ReadMode (\handle -> do
        contents <- hGetContents handle
        putStrLn contents)
-}

{-
main :: IO ()
main = do
    contents <- readFile "io-files/girlfriend.txt"
    putStrLn contents
-}

{-
main :: IO ()
main = do
    contents <- readFile "io-files/girlfriend.txt"
    writeFile "io-files/girlfriendcaps.txt" (map toUpper contents)
-}

{-
main :: IO ()
main = do
    args <- getArgs
    progName <- getProgName
    putStrLn "The arguments are:"
    mapM putStrLn args
    putStrLn "The program name is:"
    putStrLn progName
-}

main :: IO ()
main = do
    gen <- getStdGen
    putStrLn $ take 20 (randomRs ('a', 'z') gen)
    gen <- newStdGen
    putStrLn $ take 20 (randomRs ('a', 'z') gen)

shortLinesOnly :: String -> String
shortLinesOnly = unlines . filter (\l -> length l < 10) . lines

respondPalindromes :: String -> String
respondPalindromes =
    unlines .
    map (\xs -> if isPal xs then "palindrome" else "not a palindrome") .
    lines

isPal :: String -> Bool
isPal xs = xs == reverse xs

withFile' :: FilePath -> IOMode -> (Handle -> IO a) -> IO a
withFile' name mode f = bracket (openFile name mode)
    (\handle -> hClose handle)
    (\handle -> f handle)

threeCoins :: StdGen -> (Bool, Bool, Bool)
threeCoins gen =
    let (firstCoin, newGen)   = random gen
        (secondCoin, newGen') = random newGen
        (thirdCoin, newGen'') = random newGen'
    in  (firstCoin, secondCoin, thirdCoin)

finiteRandoms :: (RandomGen g, Random a) => Int -> g -> ([a], g)
finiteRandoms 0 gen = ([], gen)
finiteRandoms n gen =
    let (value, newGen)        = random gen
        (restOfList, finalGen) = finiteRandoms (n - 1) newGen
    in  (value:restOfList, finalGen)
