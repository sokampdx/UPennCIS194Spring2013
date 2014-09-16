-- UPENN CIS 194 HOMEWORK #1

-- Ex 1
toDigits :: Integer -> [Integer]
toDigits x 
    | x > 0 = (toDigits (x `div` 10)) ++ ((x `mod` 10):[])
    | otherwise = []

toDigitsRev :: Integer -> [Integer]
toDigitsRev x = reverse (toDigits x)

-- Ex 2
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther [x] = [x]
doubleEveryOther (x:xs)
    | ((length xs) `mod` 2) == 0 = x : (doubleEveryOther xs)
    | otherwise = (x * 2) : (doubleEveryOther xs)

-- Ex 3
sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x:xs)
    | x > 9 = (sumDigits (toDigits x)) + (sumDigits xs)
    | otherwise = x + (sumDigits xs)

-- Ex 4
validate :: Integer -> Bool
validate x = ((sumDigits (doubleEveryOther (toDigits x))) `mod` 10) == 0 

-- EX 5
type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi 1 x y z = (x, y):[]
hanoi n x y z = (hanoi (n-1) x z y) ++ (hanoi 1 x y z) ++ (hanoi (n-1) z y x) 


