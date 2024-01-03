import Data.List
import ListOfPrimes

largestFactor n = case find (flip divides n) primes of
    Just primeFactor -> (n, n `quot` primeFactor)
    Nothing -> (n,1)

divides d n = n `mod` d == 0

--upperLimit = 104743
upperLimit = primes!!(100-1)

largestFactors = map largestFactor [2..upperLimit]

sumFunc f l = accum 0 l
    where accum steps [] = steps
          accum steps (x:xs) = accum (steps + (f x)) xs

stepsPerN (n,d) =
      3*n + d-2
    + (n-d-1)*(6*n+2)
    + (if 2 `divides` n
       then (n-d-1)*(2*1)
       else 2 * (sumFunc (stepsBetween n) [d+1 .. n-1]))
    + 4*n + 2 * (n `quot` d) + 1
    + 1

stepsBetween n x = n `quot` x

solution = sumFunc stepsPerN largestFactors

main = putStrLn $ show $ solution

