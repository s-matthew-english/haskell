maximum' :: (Ord a) => [a] -> a
maximum' [] = error "maximum of empty list!"
maximum' [x] = x
maximum' (x:xs) = max x (maximum' xs)

replicate' :: Int -> a -> [a]
replicate' n x
    | n <= 0    = []
    | otherwise = x : replicate' (n - 1) x

take' :: Int -> [a] -> [a]
take' n _
    | n <= 0    = []
take' _ []      = []
take' n (x:xs) = x : take' (n - 1) xs

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

repeat' :: a -> [a]
repeat' x = x:repeat' x

zip' :: [a] -> [b] -> [(a, b)]
zip' _ [] = []
zip' [] _ = []
zip' (x:xs) (y:ys) = (x,y):zip' xs ys

elem' :: (Eq a) => a -> [a] -> Bool
elem' a [] = False
elem' a (x:xs)
    | a == x    = True
    | otherwise = a `elem` xs

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = 
    let smallerOrEqual = [a | a <- xs, a <= x]
        larger = [a | a <- xs, a > x]
    in quicksort smallerOrEqual ++ [x] ++ quicksort larger

multThree :: Int -> Int -> Int -> Int
multThree x y z = x * y * z

multThree' :: Int -> (Int -> (Int -> Int))
multThree' x y z = x * y * z

compareWithHundred :: Int -> Ordering
compareWithHundred x = compare 100 x

compareWithHundred' :: Int -> Ordering
compareWithHundred' = compare 100

divideByTen :: (Floating a) => a -> a
divideByTen = (/10)

isUpperAlphanum :: Char -> Bool
isUpperAlphanum = (`elem` ['A'..'Z'])

applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

flip' :: (a -> b -> c) -> (b -> a -> c)
flip' f = g
    where g x y = f y x

flip'' :: (a -> b -> c) -> (b -> (a -> c))
flip'' f = g
    where g x y = f y x

flip''' :: (a -> b -> c) -> b -> a -> c
flip''' f = g
    where g x y = f y x

map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = f x : map' f xs

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' p (x:xs)
    | p x    = x : filter' p xs
    | otherwise = filter' p xs

-- let notNull x = not (null x) in filter notNull [[1,2,3],[],[3,4,5],[2,2],[],[],[]]

-- filter' (`elem` ['a'..'z']) "u LaUgH aT mE BeCaUsE I aM diFfeRent"

-- filter' (<15) (filter' even [1..20])

-- [x | x <- [1..20], x < 15, even x]

kwicksort :: (Ord a) => [a] -> [a]
kwicksort [] = []
kwicksort (x:xs) =
    let smallerOrEqual = filter' (<= x) xs
        larger = filter' (> x) xs
    in kwicksort smallerOrEqual ++ [x] ++ kwicksort larger

largestDivisible :: Integer
largestDivisible = head (filter p [99999,99998..])
    where p x = x `mod` 3829 == 0

-- takeWhile (/= ' ') "elephants know how to party"

-- sum (takeWhile (<10000) (filter odd (map (^2) [1..])))

-- sum (takeWhile (<10000) [m | m <- [n^2 | n <- [1..]], odd m])

chain :: Integer -> [Integer]
chain 1 = [1]
chain n
    | even n = n:chain (n `div` 2)
    | odd n = n:chain (n * 3 + 1)

numLongChains :: Int
numLongChains = length (filter isLong (map chain [1..100]))
    where isLong xs = length xs > 15

-- let listOfFuns = map (*) [0..]
-- (listOfFuns !! 4) 5

numLongChains' :: Int
numLongChains' = length (filter (\xs -> length xs > 15) (map chain [1..100]))

-- map (+3) [1,6,3,2]
-- map (\x -> x + 3) [1,6,3,2]

-- zipWith (\(a,b) -> a + b) [(1,2),(3,5),(6,3),(2,6),(2,5)]

-- zipWith (\a b -> (a * 30 + 3) / b) [5,4,3,2,1] [1,2,3,4,5]
-- map (\(a,b) -> a + b) [(1,2),(3,5),(6,3),(2,6),(2,5)]

addThree''' :: Int -> Int -> Int -> Int
addThree''' x y z = x + y + z

addThree'''' :: Int -> Int -> Int -> Int
addThree'''' = \x -> \y -> \z -> x + y + z

-- flipadelphia zip ["one","two"] ["cat","dog"]
flipadelphia :: (a -> b -> c) -> b -> a -> c
flipadelphia f = \x y -> f y x

-- zipWith (flipadelphia (++)) ["love you", "love me"] ["i ", "you "]

-- (flipadelphia subtract 20) [1,2,3,4]
-- map (subtract 20) [1,2,3,4]

sum' :: (Num a) => [a] -> a
sum' xs = foldl (\acc x -> acc + x) 0 xs

sum'' :: (Num a) => [a] -> a
sum'' = foldl (+) 0






