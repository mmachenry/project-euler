import Data.Function.Memoize

f x
  | x <= 0 = 1
  | otherwise = p(x-1) + p(x-2) - f(x-3)
p x  = if odd(x) then 3 * f(x) + 1 else 3 * f(x) - 1

h :: Integer -> Integer
h = memoize g where
  g x
    | x >  0 = 3*h(x-1) + 3*h(x-2) - h(x-3)
    | otherwise = 1

cf :: Int -> Double
cf n = ((-1)^(n+1) + (2 + sqrt(3))^(n+1) + (2 - sqrt(3))^(n+1)) / 3
