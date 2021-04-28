module FoldingExercises where

-- List Exercises from Video #11
-- 1. "Create a function rev that reverses a list with only one fold."
rev :: [a] -> [a]
rev = foldl (flip (:)) []

-- 2. "Create a function prefixes that returns all the prefixes of a given list."
prefixes :: [a] -> [[a]]
prefixes = foldr (\x acc -> [x] : (map (x:) acc)) []

-- 3. "Write a function that generates an interpolation polynomial in the Lagrange form."
lagrange :: [(Float, Float)] -> Float -> Float
lagrange xs x = foldl (\acc (xj, y) -> acc + (y * l xj)) 0 xs
  where
    l xj = foldl (\acc (xk, _) -> if xj == xk then acc else acc * ((x - xk)/(xj - xk))) 1 xs

-- 4. "Create a function foldtrie that folds the elements of a trie in a preorder traversal."
data Trie a = Leaf a | Node a [Trie a]

foldtrie :: (b -> a -> b) -> b -> Trie a -> b
foldtrie f acc (Leaf x)    = f acc x
foldtrie f acc (Node x xs) = foldl f' (f acc x) xs
  where
    f' acc t = foldtrie f acc t
