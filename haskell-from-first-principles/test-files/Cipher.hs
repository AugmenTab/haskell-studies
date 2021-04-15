module Cipher where

import Data.Char

caesar :: String -> Int -> String
caesar []     _ = []
caesar (x:xs) n = (chr . (+97) $ mod ((ord x) - 97 + n) 26) : caesar xs n


unCaesar :: String -> Int -> String
unCaesar x n = caesar x (negate n)
