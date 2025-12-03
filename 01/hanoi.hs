type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi n orig dest temp
    | n == 0    = []
    | n == 1    = [(orig, dest)]
    | otherwise = hanoi (n-1) orig temp dest ++
                  [(orig, dest)] ++
                  hanoi (n-1) temp dest orig
