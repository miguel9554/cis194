toDigits :: Integer -> [Integer]
toDigits n
    | n < 10    = [n]                   -- base case: single digit
    | otherwise = toDigits (n `div` 10) ++ [n `mod` 10]

revList :: [a] -> [a]
revList [] = []
revList (x:z) = revList(z)++[x]

toDigitsRev :: Integer -> [Integer]
toDigitsRev n = revList ( toDigits (n) )

doubleEveryOtherFront :: [Integer] -> [Integer]
doubleEveryOtherFront [] = []
doubleEveryOtherFront (x:(y:z)) = x:(2*y):doubleEveryOtherFront(z)

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther xs = revList ( doubleEveryOtherFront ( revList (xs) ) )

sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits xs = sum (map sum (map toDigits xs))

validate :: Integer -> Bool
validate n = sumDigits (doubleEveryOther ( toDigits(n) )) `mod` 10 == 0
