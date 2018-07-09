maximum' :: (Ord a) => [a] -> a
maximum' = foldl1 max 

reverse' :: [a] -> [a]
reverse' = foldl (\acc x -> x : acc) []

reverse'' :: [a] -> [a]
reverse'' = foldl (flip (:)) []

product' :: (Num a) => [a] -> a
product' = foldl (*) 1

-- filter' even [1,2,3]
-- filter' odd [1,2,3]
filter' :: (a -> Bool) -> [a] -> [a]
filter' p = foldr (\x acc -> if p x then x : acc else acc) []

last' :: [a] -> a
last' = foldl1 (\_ x -> x)

-- f 3 (f 4 (f 5 (f 6 z)))
--
-- 3 + (4 + (5 + (6 + 0)))
--
-- flip (:) (flip (:) (flip (:) (flip (:) [] 3) 4) 5) 6

and' :: [Bool] -> Bool
and' xs = foldr (&&) True xs

-- scanl (+) 0 [1,2,3]
-- [0,1,3,6]

-- scanl1 (\acc x -> if x > acc then x else acc) [3,4,5,3,7,9,2,1]
-- [3,4,5,5,7,9,9,9]

-- scanl (flip (:)) [] [3,2,1]
-- [[],[3],[2,3],[1,2,3]]

sqrtSums :: Int
sqrtSums = length (takeWhile (< 1000) (scanl1 (+) (map sqrt [1..]))) + 1

-- sum (map sqrt [1..131])
-- sum (map sqrt [1..130])

-- sum (map sqrt [1..130])
-- sum $ map sqrt [1..130]

-- sqrt (3 + 4 + 9)
-- sqrt $ 3 + 4 + 9

-- sum (filter (> 10) (map (*2) [2..10]))
-- sum $ filter (>10) (map (*2) [2..10])
-- sum $ filter (>10) $ map (*2) [2..10]

-- map ($ 3) [(4 +), (10 *), (^2), (sqrt)]

-- (.) :: (b -> c) -> (a -> b) -> a -> c
-- f . g = \x -> f (g x)

-- map (\x -> negate (abs x)) [5,-3,-6,7,-3,2,-19,24]
-- map (negate . abs) [5,-3,-6,7,-3,2,-19,24] 

-- map (\xs -> negate (sum (tail xs))) [[1..5],[3..6],[1..7]]
-- map (negate . sum . tail) [[1..5],[3..6],[1..7]]

-- sum (replicate 5 (max 6.7 8.9))
-- (sum . replicate 5) (max 6.7 8.9)
-- sum . replicate 5 $ max 6.7 8.9

-- replicate 2 (product (map (*3) (zipWith max [1,2] [4,5])))
-- replicate 2 . product . map (*3) $ zipWith max [1,2] [4,5]

-- zipWith max [1,2] [4,5]
-- map (*3) $ max [1,2] [4,5]
-- map (*3) $ zipWith max [1,2] [4,5]
-- product . map (*3) $ max [1,2] [4,5]
-- product . map (*3) $ zipWith max [1,2] [4,5]
-- replicate 2 . map (*3) $ zipWith max [1,2] [4,5]
-- replicate 2 . map (*3) $ max [1,2] [4,5]

sum' :: (Num a) => [a] -> a
sum' xs = foldl (+) 0 xs

sum'' :: (Num a) => [a] -> a
sum'' = foldl (+) 0

-- let fn x = ceiling (negate (tan (cos (max 50 x))))
-- fn = ceiling . negate. tan . cos . max 50

oddSquareSum :: Integer
oddSquareSum = sum (takeWhile (<10000) (filter odd (map (^2) [1..])))

oddSquareSum' :: Integer
oddSquareSum' = sum . takeWhile (<10000) . filter odd $ map (^2) [1..]







