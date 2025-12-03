module Golf where

-- Exercise 1
takeEveryNth :: [a] -> Int -> [a]
takeEveryNth l n = map snd (filter (\(i,_) -> (i+1) `mod` n == 0) (zip [0..] l))

skips :: [a] -> [[a]]
skips xs = map (\n ->takeEveryNth xs n) [1..length xs]

-- Exercise 2
localMaxima :: [Integer] -> [Integer]
localMaxima xs =    map (\(_,a,_) -> a) $
                    filter (\(prev,cur,next) -> cur > prev && cur > next) $
                    zip3 xs (tail xs) (tail (tail xs))
-- Exercise 3
countOccurences :: [Integer] -> Integer -> Integer
countOccurences xs n = fromIntegral $ length $ filter (==n) xs

generateHistogram :: [Integer] -> [(Integer, Integer)]
generateHistogram xs = map (\n -> (n, countOccurences xs n)) [1..9]

histogramLine :: [(Integer, Integer)] -> Integer -> String
histogramLine xs line = map (\(val,count) -> if count >= line then '*' else ' ') xs

histogramPlot :: [(Integer, Integer)] -> String
histogramPlot xs = unlines $ reverse $ map (\n -> histogramLine xs n) [1..(maximum $ map snd xs)]

histogramTail :: String
histogramTail = "=========\n123456789\n"

histogram :: [Integer] -> String
histogram xs = histogramPlot (generateHistogram xs) ++ histogramTail
