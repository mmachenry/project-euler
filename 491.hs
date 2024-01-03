import Data.List

waysToSum :: Int -> [Int] -> Int
waysToSum 0 [] = 1
waysToSum _ [] = 0
waysToSum n ds = sum $ map (\x->waysToSum (n-x) (delete x ds)) ds

-- [(1,89),(12,78),(23,67),(34,56),(45,45)]
nums = [ (a,b) |
    a <- [0..90], b <- [a..90],
    a + b == 90, (a - b) `mod` 11 == 0]


