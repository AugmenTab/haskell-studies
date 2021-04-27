module ListExercises where

import Data.List

-- List Exercises from Video #5
{- 1. "Create a function elem that returns True if an element is in a given list and returns False
otherwise." -}
myElem :: (Eq a) => a -> [a] -> Bool
myElem _ []     = False
myElem e (x:xs) = e == x || myElem e xs

-- 2. "Create a function nub that removes all duplicates from a given list."
myNub :: (Eq a) => [a] -> [a]
myNub []     = []
myNub (x:xs) = if elem x xs then myNub xs else x : nub xs

{- 3. "Create a function isAsc that returns True if the list given to it is a list of ascending
order." -}
isAsc :: [Int] -> Bool
isAsc (x:xs) = null xs || (x <= head xs && isAsc xs)

{- 4. "Create a function hasPath that determines if a path from one node to another exists within a
directed graph." I used a helper function to make pairs from a list of Ints. This helper function
takes a list of the numbers between the original from and to values and makes all the necessary
tuple elements to form the path. Then, it checks to see if that list is a subset of the original
graph. My solution works for the test conditions given in the exercise, but it would not work for
graphs where the ends are connected. -}
makePairs :: [Int] -> [(Int, Int)]
makePairs (x:y:xs) = if null xs then p : [] else p : (makePairs (y:xs)) where p = (x, y)

hasPath :: [(Int, Int)] -> Int -> Int -> Bool
hasPath graph from to = x == intersect x graph || y == intersect y graph
  where
    p = [(min from to)..(max from to)]
    x = makePairs p
    y = makePairs $ reverse p

-- The video solution:
hasPath' :: [(Int,Int)] -> Int -> Int -> Bool
hasPath' [] x y = x == y
hasPath' xs x y
  | x == y    = True
  | otherwise =
    let
      xs' = [ (n,m) | (n,m) <- xs, n /= x ]
    in
      or [ hasPath' xs' m y | (n, m) <- xs, n == x ]
