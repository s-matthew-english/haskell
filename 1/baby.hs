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