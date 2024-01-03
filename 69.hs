import Math.Sieve.Phi
import Data.Function
upperBound = 1000000
s = sieve upperBound
f n = (fromIntegral n) / (fromIntegral (phi s n))
solution = maximumSnd (map (\n->(n, f n)) [2..upperBound])
maximumSnd l = foldl mos (head l) (tail l)
mos a b = if snd a > snd b then a else b
