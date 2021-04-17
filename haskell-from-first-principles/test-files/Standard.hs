module Standard where

-- 1. myOr returns True if any Bool in the list is True:
myOr :: [Bool] -> Bool
myOr []     = False
myOr (x:xs) = x || myOr xs

-- 2. myAny returns True if (a -> Bool) applied to any of the values in the list returns True:
myAny :: (a -> Bool) -> [a] -> Bool
myAny _ []     = False
myAny f (x:xs) = f x || myAny f xs

-- 3. Write a recursive myElem (concrete), then write another that uses any.
myElem :: Eq a => a -> [a] -> Bool
myElem _ []     = False
myElem n (x:xs) = n == x || myElem n xs

myElemAny :: Eq a => a -> [a] -> Bool
myElemAny x = myAny (x==)

-- 4. Implement myReverse:
myReverse :: [a] -> [a]
myReverse []     = []
myReverse (x:xs) = myReverse xs ++ [x]

-- 5. squish flattens a list of lists into a list:
squish :: [[a]] -> [a]
squish []      = []
squish (x:xs)  = x ++ squish xs

-- 6. squishMap maps a function over a list and concatenates the results:
squishMap :: (a -> [b]) -> [a] -> [b]
squishMap _ []     = []
squishMap f (x:xs) = f x ++ squishMap f xs

-- 7. squishAgain flattens a list of lists into a list. This time, reuse the squishMap function:
squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

{- 8. myMaximumBy takes a comparison function and a list and returns the greatest element of the
list based on the last value that the comparison returns GT for. -}
myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy f (x:xs) = go f xs x
  where go _ []     acc = acc
        go g [y]    acc = case g y acc of
                               GT -> y
                               _  -> acc
        go g (y:ys) acc = case g y acc of
                               GT -> go g ys y
                               _  -> go g ys acc

{- 9. myMinimumBy takes a comparison function and a list and returns the least element of the list
based on the last value that the comparison returns LT for. -}
myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy f (x:xs) = go f xs x
  where go _ []     acc = acc
        go g [y]    acc = case g y acc of
                               LT -> y
                               _  -> acc
        go g (y:ys) acc = case g y acc of
                               LT -> go g ys y
                               _  -> go g ys acc

{- 10. Using the myMinimumBy and myMaximumBy functions, write your own version of minimum and
maximum. -}
maximum2 :: (Ord a) => [a] -> a
maximum2 = myMaximumBy compare

minimum2 :: (Ord a) => [a] -> a
minimum2 = myMinimumBy compare
