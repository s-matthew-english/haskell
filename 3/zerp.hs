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