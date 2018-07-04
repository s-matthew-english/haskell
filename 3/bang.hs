lucky :: Int -> String
lucky 7 = "LUCKY NUMBER SEVEN"
lucky x = "Sorry, you're out of luck, pal!"

sayMe :: Int -> String
sayMe 1 = "One!"
sayMe 2 = "Two!"
sayMe 3 = "Three!"
sayMe 4 = "Four!"
sayMe 5 = "Five!"
sayMe x = "Not between 1 and 5!"

faktorial :: Int -> Int
faktorial 0 = 1
faktorial n = n * faktorial (n - 1)

charName :: Char -> String
charName 'a' = "Albert"
charName 'b' = "Broseph"
charName 'c' = "Cecil"

addVectors :: (Double, Double) -> (Double, Double) -> (Double, Double)
addVectors a b = (fst a + fst b, snd a + snd b)

addVectors' :: (Double, Double) -> (Double, Double) -> (Double, Double)
addVectors' (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

first :: (a, b, c) -> a
first (x, _, _) = x

second :: (a, b, c) -> b
second (_, y, _) = y

third :: (a, b, c) -> c
third (_, _, z) = z

xs = [(1,3),(4,3),(2,4),(5,3),(5,6),(3,1)]
-- [a+b | (a,b) <- xs]

head' :: [a] -> a
head' [] = error "Can't call head on an empty list, dummy!"
head' (x:_) = x

tell :: (Show a) => [a] -> String
tell [] = "The list is empty"
tell (x:[]) = "The list has one element: " ++ show x
tell (x:y:[]) = "The list has two elements: " ++ show x ++ show y
tell (x:y:_) = "The list is long. The first two elements are: " ++ show x ++ " and " ++ show y

badAdd :: (Num a) => [a] -> a
badAdd (x:y:z:[]) = x + y + z

firstLetter :: String -> String
firstLetter "" = "Empty string, whoops!"
firstLetter all@(x:xs) = "The first letter of " ++ all ++ " is " ++ [x]

bmiTell :: Double -> String
bmiTell bmi
	| bmi <= 18.5 = "You're underweight, eat more!"
	| bmi <= 25.0 = "Looking good!"
	| bmi <= 30.0 = "You're overweight. Let's work out together!"
	| otherwise = "You're obese. Go see a doctor."
