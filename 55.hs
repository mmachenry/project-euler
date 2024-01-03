import Debug.Trace

debug x = trace (show x) x

isLychrel :: Integer -> Bool
isLychrel num = not $ check 50 num
    where check 0 n = False
          check steps n =
              let next = n + (reverseNum n)
              in isPalendromNum next || check (steps-1) next

reverseNum :: Integer -> Integer
reverseNum = read . reverse . show

isPalendromNum :: Integer -> Bool
isPalendromNum n = n == (reverseNum n)

solve upperLimit = length $ filter id $ map isLychrel [0 .. upperLimit]
solution = solve 10000
main = putStrLn $ show solution
