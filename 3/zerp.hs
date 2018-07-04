bmiTell :: Double -> Double -> String
bmiTell weight height
    | weight / height ^ 2 <= 18.5 = "You're underweight, eat more!"
    | weight / height ^ 2 <= 25.0 = "Looking good!"
    | weight / height ^ 2 <= 30.0 = "You're overweight. Let's work out together!"
    | otherwise = "You're obese. Go see a doctor."

max' :: (Ord a) => a -> a -> a
max' a b
    | a <= b    = b
    | otherwise = a

myCompare :: (Ord a) => a -> a -> Ordering
a `myCompare` b
    | a == b    = EQ
    | a <= b    = LT 
    | otherwise = GT

bmiTell' :: Double -> Double -> String
bmiTell' weight height
    | bmi <= skinny = "You're underweight, eat more!"
    | bmi <= normal = "Looking good!"
    | bmi <= overweight = "You're overweight. Let's work out together!"
    | otherwise = "You're obese. Go see a doctor."
    where bmi = weight / height ^ 2
          skinny = 18.5
          normal = 25.0
          overweight = 30.0

bmiTell'' :: Double -> Double -> String
bmiTell'' weight height
    | bmi <= 18.5 = "You're underweight, eat more!"
    | bmi <= 25.0 = "Looking good!"
    | bmi <= 30.0 = "You're overweight. Let's work out together!"
    | otherwise = "You're obese. Go see a doctor."
    where bmi = weight / height ^ 2

bmiTell''' :: Double -> Double -> String
bmiTell''' weight height
    | bmi <= skinny = "You're underweight, eat more!"
    | bmi <= normal = "Looking good!"
    | bmi <= overweight = "You're overweight. Let's work out together!"
    | otherwise = "You're obese. Go see a doctor."
    where bmi = weight / height ^ 2
          (skinny, normal, overweight) = (18.5, 25.0, 30.0)

badGreeting :: String
badGreeting = "Oh! Pffft. It's you."

niceGreeting :: String
niceGreeting = "Hello! So very nice to see you,"

greet :: String -> String
greet "Juan" = niceGreeting ++ " Juan!"
greet "Fernando" = niceGreeting ++ " Fernando!"
greet name = badGreeting ++ " " ++ name

-- greet' :: String -> String
-- greet' "Juan" = niceGreeting ++ " Juan!"
-- greet' "Fernando" = niceGreeting ++ " Fernando!"
-- greet' name = badGreeting ++ " " ++ name
--   where niceGreeting = "Hello! So very nice to see you,"
--         badGreeting = "Oh! Pffft. It's you."

initials :: String -> String -> String
initials firstname lastname = [f] ++ ". " ++ [l] ++ "."
    where (f:_) = firstname
          (l:_) = lastname

calcBmis :: [(Double, Double)] -> [Double]
calcBmis xs = [bmi w h | (w, h) <- xs]
    where bmi weight height = weight / height ^ 2

calcBmis' :: [(Double, Double)] -> [Double]
calcBmis' xs = [bmi | (w, h) <- xs, let bmi = w / h ^ 2]

calcBmis'' :: [(Double, Double)] -> [Double]
calcBmis'' xs = [bmi | (w, h) <- xs, let bmi = w / h ^ 2, bmi > 25.0]

-- 4 * (let a = 9 in a + 1) + 2
--
-- [let square x = x * x in (square 5, square 3, square 2)]
--
-- (let a = 100; b = 200; c = 300 in a * b * c, let foo = "Hey "; bar = "there!" in foo ++ bar)
--
-- (let (a, b, c) = (1, 2, 3) in a + b + c) * 100

cylinder :: Double -> Double -> Double
cylinder r h =
    let sideArea = 2 * pi * r * h
        topArea = pi * r ^ 2
    in sideArea + 2 * topArea

-- λ: let zoot x y z = x * y + z
-- λ: zoot 3 9 2
--
-- λ: let boot x y z = x * y + z in boot 3 4 2

head' :: [a] -> a
head' [] = error "No head for empty lists!"
head' (x:_) = x

head'' :: [a] -> a
head'' xs = case xs of [] -> error "No head for empty lists!"
                       (x:_) -> x

describeList :: [a] -> String
describeList ls = "The list is " ++ case ls of [] -> "empty."
                                               [x] -> "a singleton list."
                                               xs -> "a longer list."

describeList' :: [a] -> String
describeList' ls = "The list is " ++ what ls
    where what [] = "empty."
          what [x] = "a singleton list."
          what xs = "a longer list."







