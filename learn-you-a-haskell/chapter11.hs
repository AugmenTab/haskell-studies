-- chapter11
module Chapter11 where

import Control.Applicative (liftA2)
-- import Data.Char
-- import Data.List


-- Not a functor, because it disobeys the second functor law.
data CMaybe a = CNothing | CJust Int a deriving (Show)

instance Functor CMaybe where
    fmap f CNothing          = CNothing
    fmap f (CJust counter x) = CJust (counter + 1) (f x)


{-
main :: IO ()
main = do
    line <- getLine
    let line' = reverse line
    putStrLn $ "You said " ++ line' ++ " backwards!"
    putStrLn $ "Yes, you said " ++ line' ++ " backwards!"
-}

{-
main :: IO ()
main = do
    line <- fmap reverse getLine
    putStrLn $ "You said " ++ line ++ " backwards!"
    putStrLn $ "Yes, you said " ++ line ++ " backwards!"
-}

{-
main :: IO ()
main = do
    line <- fmap (intersperse '-' . reverse . map toUpper) getLine
    putStrLn line
-}

myAction :: IO String
myAction = do
    a <- getLine
    b <- getLine
    return $ a ++ b

myAction' :: IO String
myAction' = (++) <$> getLine <*> getLine

main :: IO ()
main = do
    a <- (++) <$> getLine <*> getLine
    putStrLn $ "The two lines concatenated turn out to be: " ++ a

sequenceA' :: Applicative f => [f a] -> f [a]
sequenceA' []    = pure []
sequenceA' (x:xs) = (:) <$> x <*> sequenceA' xs

sequenceA'' :: Applicative f => [f a] -> f [a]
sequenceA'' = foldr (liftA2 (:)) (pure [])
