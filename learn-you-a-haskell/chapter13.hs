-- chapter13: A Fistful of Monads
module Chapter13 where

import Control.Monad (guard)


type Birds = Int
type Pole  = (Birds, Birds)

type KnightPos = (Word, Word)


applyMaybe :: Maybe a -> (a -> Maybe b) -> Maybe b
applyMaybe Nothing  _ = Nothing
applyMaybe (Just x) f = f x

landLeft :: Birds -> Pole -> Maybe Pole
landLeft n (left, right)
    | abs ((left + n) - right) < 4 = Just (left + n, right)
    | otherwise                    = Nothing

landRight :: Birds -> Pole -> Maybe Pole
landRight n (left, right)
    | abs ((right + n) - left) < 4 = Just (left, right + n)
    | otherwise                    = Nothing

x -: f = f x

banana :: Pole -> Maybe Pole
banana _ = Nothing

foo :: Maybe String
foo = Just 3   >>= (\x ->
      Just "!" >>= (\y ->
      Just (show x ++ y)))

foo' :: Maybe String
foo' = do
    x <- Just 3
    y <- Just "!"
    Just (show x ++ y)

marySue :: Maybe Bool
marySue = do
    x <- Just 9
    Just (x > 8)

routine :: Maybe Pole
routine = do
    start  <- return (0, 0)
    first  <- landLeft  2 start
    second <- landRight 2 first
    landLeft 1 second

routine' :: Maybe Pole
routine' = do
    start <- return (0, 0)
    first <- landLeft 2 start
    Nothing
    second <- landRight 2 first
    landLeft 1 second

foldRoutine :: Maybe Pole
foldRoutine = foldl (>>=) (return (0,0)) [landLeft 2, landRight 2, landLeft 1]

foldRoutine' :: Maybe Pole
foldRoutine' = foldl (>>=) (return (0,0))
                [landLeft 2, banana, landRight 2, landLeft 1]

justH :: Maybe Char
justH = do
    (x:xs) <- Just "hello"
    return x

wopwop :: Maybe Char
wopwop = do
    (x:xs) <- Just ""
    return x

listOfTuples :: [(Int, Char)]
listOfTuples = do
    n  <- [1, 2]
    ch <- ['a', 'b']
    return (n, ch)

-- [ x | x <- [1..50], elem '7' $ show x ]
sevensOnly :: [Int]
sevensOnly = do
    x <- [1..50]
    guard (elem '7' $ show x)
    return x

moveKnight :: KnightPos -> [KnightPos]
moveKnight (c, r) = do
    (c', r') <- [ (c + 2, r - 1), (c + 2, r + 1), (c - 2, r - 1), (c - 2, r + 1)
                , (c + 1, r - 2), (c + 1, r + 2), (c - 1, r - 2), (c - 1, r + 2)
                ]
    guard (elem c' [1..8] && elem r' [1..8])
    return (c', r')

moveKnight' :: KnightPos -> [KnightPos]
moveKnight' (c, r) = filter onBoard
    [ (c + 2, r - 1), (c + 2, r + 1), (c - 2, r - 1), (c - 2, r + 1)
    , (c + 1, r - 2), (c + 1, r + 2), (c - 1, r - 2), (c - 1, r + 2)
    ]
  where onBoard (c, r) = elem c [1..8] && elem r [1..8]

in3 :: KnightPos -> [KnightPos]
in3 start = do
    first  <- moveKnight start
    second <- moveKnight first
    moveKnight second

canReachIn3 :: KnightPos -> KnightPos -> Bool
canReachIn3 start end = elem end $ in3 start

(<=<) :: (Monad m) => (b -> m c) -> (a -> m b) -> (a -> m c)
f <=< g = (\x -> g x >>= f)
