-- chapter12: Monoids
module Chapter12 where

import Chapter07 (Tree (EmptyTree, Node))
import qualified Data.Foldable as F


newtype CharList = CharList { getCharList :: [Char] } deriving (Eq, Show)

newtype Pair b a = Pair { getPair :: (a, b) }

instance Functor (Pair c) where
    fmap f (Pair (x, y)) = Pair (f x, y)

newtype CoolBool = CoolBool { getCoolBool :: Bool }

instance F.Foldable Tree where
    foldMap f EmptyTree    = mempty
    foldMap f (Node x l r) = F.foldMap f l `mappend`
                             f x           `mappend`
                             F.foldMap f r


helloMe :: CoolBool -> String
helloMe (CoolBool _) = "hello"

lengthCompare :: String -> String -> Ordering
lengthCompare x y = let a = length x `compare` length y
                        b = compare x y
                    in  if a == EQ then b else a

lengthCompare' :: String -> String -> Ordering
lengthCompare' x y = mappend (length x `compare` length y) (compare x y)

lengthCompare'' :: String -> String -> Ordering
lengthCompare'' x y = (length x `compare` length y) `mappend`
                      (vowels x `compare` vowels y) `mappend`
                      (compare x y)
  where vowels = length . filter (`elem` "aeiou")

testTree = Node 5
            (Node 3
                (Node 1 EmptyTree EmptyTree)
                (Node 6 EmptyTree EmptyTree)
            )
            (Node 9
                (Node 8 EmptyTree EmptyTree)
                (Node 10 EmptyTree EmptyTree)
            )
