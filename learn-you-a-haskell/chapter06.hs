-- chapter06: Modules
module Chapter06 where

import Data.Char
import Data.List
import qualified Data.Map as Map


numUniques :: (Eq a) => [a] -> Int
numUniques = length . nub

wordNums :: String -> [(String, Int)]
wordNums = map (\ws -> (head ws, length ws)) . group . sort . words

isIn :: (Eq a) => [a] -> [a] -> Bool
isIn x y = any (x `isPrefixOf`) (tails y)

encode :: Int -> String -> String
encode offset msg = map (chr . (+ offset) . ord) msg

decode :: Int -> String -> String
decode shift msg = encode (negate shift) msg

digitSum :: Int -> Int
digitSum = sum . map digitToInt . show

firstTo40 :: Maybe Int
firstTo40 = find (\x -> digitSum x == 40) [1..]

firstTo :: Int -> Maybe Int
firstTo n = find (\x -> digitSum x == n) [1..]

phoneBook :: [(String, String)]
phoneBook =
    [ ("betty",   "555-2938")
    , ("betty",   "342-2492")
    , ("bonnie",  "452-2928")
    , ("patsy",   "493-2928")
    , ("patsy",   "943-2929")
    , ("patsy",   "827-9162")
    , ("lucille", "205-2928")
    , ("wendy",   "939-8282")
    , ("penny",   "853-2492")
    , ("penny",   "555-2111")
    ]

findKey :: (Eq k) => k -> [(k, v)] -> Maybe v
findKey key [] = Nothing
findKey key ((k,v):xs)
    | key == k  = Just v
    | otherwise = findKey key xs

findKey' :: (Eq k) => k -> [(k, v)] -> Maybe v
findKey' key xs = foldr (\(k, v) acc -> if key == k then Just v else acc) Nothing xs

phoneBook' :: Map.Map String String
phoneBook' = Map.fromList phoneBook

stringToDigits :: String -> [Int]
stringToDigits = map digitToInt . filter isDigit

phoneBookToMap :: (Ord k) => [(k, a)] -> Map.Map k [a]
phoneBookToMap xs = Map.fromListWith (++) $ map (\(k, v) -> (k, [v])) xs
