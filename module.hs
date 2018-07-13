import Data.List

numUniques :: (Eq a) => [a] -> Int
numUniques = length . nub

-- \xs -> length (nub xs)
numUniques' :: (Eq a) => [a] -> Int
numUniques' = \xs -> length (nub xs)

-- :m + Data.List
-- :m + Data.List Data.Map Data.Set
-- import Data.List (nub, sort)
-- import Data.List hiding (nub)
-- import qualified Data.Map
-- import qualified Data.Map as M

-- words "hey these              are the words in this sentence"

-- group ["boom","bip","bip","boom","boom"]
-- sort ["boom","bip","bip","boom","boom"]

wordNums :: String -> [(String,Int)]
wordNums = map (\ws -> (head ws, length ws)) . group . sort . words

-- let test = "wa wa wee wa"
-- wordNums test

wordNums' :: String -> [(String,Int)]
wordNums' xs = map (\ws -> (head ws, length ws)) (group (sort (words xs)))
