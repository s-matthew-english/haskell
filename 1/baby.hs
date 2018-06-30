doubleMe x = x + x

doubleUs0 x y = x * 2 + y * 2

doubleUs1 x y = doubleMe x + doubleMe y

-- Haskell's `if` is an _expression_ that
-- must return a value, and not a statement
doubleSmallNumber0 x = if x > 100
					     then x
					     else x * 2

doubleSmallNumber1 x = (if x > 100 then x else x * 2) + 1

conanO'Brien = "It's a-me, Conan O'Brien!"

-- Using the `let` keyword to define a name in GHCi. 
-- Entering `let a = 1` in GHCi is equivalent to
-- writing `a = 1` in a script, then loading it with
-- :l
a = 1

matt = [1, 2, 3, 4]
hew = [9, 10, 11, 12]

-- In Haskell, string are really just lists of characters.
-- For example, the string "hello" is actually the same as 
-- the list `['h', 'e', 'l', 'l', 'o']`. Because of this, we
-- can use list functions on strings, which is really handy.
eq1 = "hello"
eq2 = ['h', 'e', 'l', 'l', 'o']

length' xs = sum [1 | _ <- xs]


removeNonUppercase st = [c | c <- st, c `elem` ['A'..'Z']]

testFunction sme = [ (a,b,c) | c <- [1..sme], a <- [1..c], b <- [1..a], a^2 + b^2 == c^2, a+b+c == 24]






