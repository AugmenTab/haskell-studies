-- chapter14: For a Few Monads More
module Chapter14  where

import Control.Monad.Fail
import Control.Monad.State
import Control.Monad.Writer.Lazy
import Data.List
import Data.Monoid
import Data.Ratio
import System.Random
import qualified Data.DList as DList

import Chapter13 (KnightPos, moveKnight)


data Coin = Heads | Tails deriving (Show, Eq)

newtype Prob a = Prob { getProb :: [(a, Rational)] } deriving Show

instance Functor Prob where
    fmap f (Prob xs) = Prob $ map (\(x, p) -> (f x, p)) xs

instance Applicative Prob where
    pure  = return
    (<*>) = ap

instance Monad Prob where
    return x = Prob [(x, 1 % 1)]
    m >>= f  = flatten (fmap f m)
    -- fail _   = Prob []


type Food  = String
type Price = Sum Int
type Stack = [Int]


isBigGang :: Int -> (Bool, String)
isBigGang x = (x > 9, "Compared gane size to 9.")

applyLog :: (a, [c]) -> (a -> (b, [c])) -> (b, [c])
applyLog (x, log) f = let (y, newLog) = f x in (y, mappend log newLog)

addDrink :: Food -> (Food, Price)
addDrink "beans" = ("milk"   , Sum 25)
addDrink "jerky" = ("whiskey", Sum 99)
addDrink _       = ("beer"   , Sum 30)

logNumber :: Int -> Writer [String] Int
logNumber x = writer (x, ["Got number: " ++ show x])

multWithLog :: Writer [String] Int
multWithLog = do
    a <- logNumber 3
    b <- logNumber 5
    tell ["Gonna multiply these two"]
    return (a * b)

gcd' :: Int -> Int -> Writer [String] Int
gcd' a b
    | b == 0    = do
        tell ["Finished with " ++ show a]
        return a
    | otherwise = do
        tell [show a ++ " mod " ++ show b ++ " = " ++ show (mod a b)]
        gcd' b (mod a b)

gcdReverse :: Int -> Int -> Writer [String] Int
gcdReverse a b
    | b == 0    = do
        tell ["Finished with " ++ show a]
        return a
    | otherwise = do
        result <- gcdReverse b (mod a b)
        tell [show a ++ " mod " ++ show b ++ " = " ++ show (mod a b)]
        return result

-- Couldn't get book example of DiffList to compile. Imported DList instead.
gcdReverse' :: Int -> Int -> Writer (DList.DList String) Int
gcdReverse' a b
    | b == 0    = do
        tell (DList.fromList ["Finished with " ++ show a])
        return a
    | otherwise = do
        result <- gcdReverse' b (mod a b)
        tell (DList.fromList 
                [show a ++ " mod " ++ show b ++ " = " ++ show (mod a b)]
             )
        return result

finalCountDown :: Int -> Writer (DList.DList String) ()
finalCountDown 0 = do
    tell (DList.fromList ["0"])
finalCountDown x = do
    finalCountDown (x - 1)
    tell (DList.fromList [show x])

addStuff :: Int -> Int
addStuff = do
    a <- (*2)
    b <- (+10)
    return (a + b)

pop :: Stack -> (Int, Stack)
pop (x:xs) = (x, xs)

pop' :: State Stack Int
pop' = state $ \(x:xs) -> (x, xs)

push :: Int -> Stack -> ((), Stack)
push a xs = ((), a:xs)

push' :: Int -> State Stack ()
push' a = state $ \xs -> ((), a:xs)

stackManip :: Stack -> (Int, Stack)
stackManip stack =
    let ((), newStack1) = push 3 stack
        (a , newStack2) = pop newStack1
    in  pop newStack2

stackManip' :: State Stack Int
stackManip' = do
    push' 3
    a <- pop'
    pop'

stackStuff :: State Stack ()
stackStuff = do
    a <- pop'
    if a == 5
        then push' 5
        else do
            push' 3
            push' 8

moreStack :: State Stack ()
moreStack = do
    a <- stackManip'
    if a == 100
        then stackStuff
        else return ()

stackyStack :: State Stack ()
stackyStack = do
    stackNow <- get
    if stackNow == [1, 2, 3]
        then put [8, 3, 1]
        else put [9, 2, 1]

pop'' :: State Stack Int
pop'' = do
    xs <- get
    put (tail xs)
    return (head xs)

push'' :: Int -> State Stack ()
push'' x = do
    xs <- get
    put (x:xs)

randomSt :: (RandomGen g, Random a) => State g a
randomSt = state random

threeCoins :: State StdGen (Bool, Bool, Bool)
threeCoins = do
    a <- randomSt
    b <- randomSt
    c <- randomSt
    return (a, b, c)

keepSmall :: Int -> Writer [String] Bool
keepSmall x
    | x < 4     = do
        tell ["Keeping " ++ show x]
        return True
    | otherwise = do
        tell [show x ++ " is too large, throwing it away"]
        return False

powerset :: [a] -> [[a]]
powerset xs = filterM (\x -> [True, False]) xs

binSmalls :: Int -> Int -> Maybe Int
binSmalls acc x
    | x > 9     = Nothing
    | otherwise = Just (acc + x)

solveRPN :: String -> Maybe Double
solveRPN st = do
    [result] <- foldM foldingFunction [] (words st)
    return result

foldingFunction :: [Double] -> String -> Maybe [Double]
foldingFunction (x:y:xs) "*"          = return ((y * x) : xs)
foldingFunction (x:y:xs) "+"          = return ((y + x) : xs)
foldingFunction (x:y:xs) "-"          = return ((y - x) : xs)
foldingFunction xs       numberString = liftM (:xs) (readMaybe numberString)

readMaybe :: (Read a) => String -> Maybe a
readMaybe st = case reads st of
    [(x, "")] -> Just x
    _         -> Nothing

inMany :: Int -> KnightPos -> [KnightPos]
inMany x start = return start >>= foldr (<=<) return (replicate x moveKnight)

canReachIn :: Int -> KnightPos -> KnightPos -> Bool
canReachIn x start end = elem end (inMany x start)

thisSituation :: Prob (Prob Char)
thisSituation = Prob
    [ (Prob [('a', 1 % 2), ('b', 1 % 2)], 1 % 4)
    , (Prob [('c', 1 % 2), ('d', 1 % 2)], 3 % 4)
    ]

flatten :: Prob (Prob a) -> Prob a
flatten (Prob xs) = Prob $ concatMap multAll xs
  where multAll (Prob innerxs, p) = map (\(x, r) -> (x, p * r)) innerxs

coin :: Prob Coin
coin = Prob [(Heads, 1 % 2), (Tails, 1 % 2)]

loadedCoin :: Prob Coin
loadedCoin = Prob [(Heads, 1 % 10), (Tails, 9 % 10)]

flipThree :: Prob Bool
flipThree = do
    a <- coin
    b <- coin
    c <- loadedCoin
    return (all (== Tails) [a, b, c])
