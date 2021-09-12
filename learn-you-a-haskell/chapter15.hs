-- chapter15: Zippers
module Chapter15 where

import Chapter13 ((-:))


data Crumb a = LeftCrumb a (Tree a) | RightCrumb a (Tree a) deriving (Show)
data Direction = L | R deriving (Show)
data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show)


--type Breadcrumbs = [Direction]
type Breadcrumbs a = [Crumb a]
type Directions = [Direction]
type ListZipper a = ([a], [a])
type Zipper a = (Tree a, Breadcrumbs a)


freeTree :: Tree Char
freeTree =
    Node 'P'
        (Node 'O'
            (Node 'L'
                (Node 'N' Empty Empty)
                (Node 'T' Empty Empty)
            )
            (Node 'Y'
                (Node 'S' Empty Empty)
                (Node 'A' Empty Empty)
            )
        )
        (Node 'L'
            (Node 'W'
                (Node 'C' Empty Empty)
                (Node 'R' Empty Empty)
            )
            (Node 'A'
                (Node 'A' Empty Empty)
                (Node 'C' Empty Empty)
            )
        )

changeToP :: Directions -> Tree Char -> Tree Char
changeToP (L:ds) (Node x l r) = Node x (changeToP ds l) r
changeToP (R:ds) (Node x l r) = Node x l (changeToP ds r)
changeToP []     (Node _ l r) = Node 'P' l r

elemAt :: Directions -> Tree a -> a
elemAt (L:ds) (Node _ l _) = elemAt ds l
elemAt (R:ds) (Node _ _ r) = elemAt ds r
elemAt []     (Node x _ _) = x

{-
goLeft :: (Tree a, Breadcrumbs) -> (Tree a, Breadcrumbs)
goLeft (Node _ l _, bs) = (l, L:bs)

goRight :: (Tree a, Breadcrumbs) -> (Tree a, Breadcrumbs)
goRight (Node _ _ r, bs) = (r, R:bs)
-}

{-
goLeft :: Zipper a -> Zipper a
goLeft (Node x l r, bs) = (l, (LeftCrumb x r) : bs)

goRight :: Zipper a -> Zipper a
goRight (Node x l r, bs) = (r, (RightCrumb x l) : bs)

goUp :: Zipper a -> Zipper a
goUp (t, (LeftCrumb  x r):bs) = (Node x t r, bs)
goUp (t, (RightCrumb x l):bs) = (Node x l t, bs)
-}

modify :: (a -> a) -> Zipper a -> Zipper a
modify f (Node x l r, bs) = (Node (f x) l r, bs)
modify _ (Empty,      bs) = (Empty, bs)

attach :: Tree a -> Zipper a -> Zipper a
attach t (_, bs) = (t, bs)

{-
topMost :: Zipper a -> Zipper a
topMost (t, []) = (t, [])
topMost z       = topMost (goUp z)
-}

goForward :: ListZipper a -> ListZipper a
goForward (x:xs, bs) = (xs, x:bs)

goBack :: ListZipper a -> ListZipper a
goBack (xs, b:bs) = (b:xs, bs)

goLeft :: Zipper a -> Maybe (Zipper a)
goLeft (Node x l r, bs) = Just (l, LeftCrumb x r:bs)
goLeft (Empty     , _)  = Nothing

goRight :: Zipper a -> Maybe (Zipper a)
goRight (Node x l r, bs) = Just (r, RightCrumb x l:bs)
goRight (Empty     , _)  = Nothing

goUp :: Zipper a -> Maybe (Zipper a)
goUp (t, LeftCrumb  x r:bs) = Just (Node x t r, bs)
goUp (t, RightCrumb x l:bs) = Just (Node x l t, bs)
goUp (_, []               ) = Nothing

{-
ghci> let coolTree = Node 1 (Empty (Node 3 Empty Empty))
ghci> foldl (>>=) (return (coolTree, [])) $ replicate 1 goRight
Just (Node 3 Empty Empty,[RightCrumb 1 Empty])
ghci> foldl (>>=) (return (coolTree, [])) $ replicate 2 goRight
Just (Empty,[RightCrumb 3 Empty,RightCrumb 1 Empty])
ghci> foldl (>>=) (return (coolTree, [])) $ replicate 3 goRight
Nothing
-}
