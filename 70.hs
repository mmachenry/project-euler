import Math.Sieve.Phi
import Data.Function
import List
upperBound = 10^7
s = sieve upperBound
solution = minimumSnd $
    filter isPermute $
    (map (\n->(n, phi s n)) [2..upperBound])
isPermute (n,m) = sort (show n) == sort (show m)
minimumSnd l = foldl mos (head l) (tail l)
f (n,p) = (fromIntegral n) / (fromIntegral p)
mos a b = if f a > f b then b else a
main = putStrLn $ show solution
