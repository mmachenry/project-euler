main = putStrLn $ show (and tests)

tests = [ t x y z n == s x y z n |
    x <- [1..5],
    y <- [1..5],
    z <- [1..3],
    n <- [1..3]
    ]

tOld x y z n = layer n
    where layer n
              | n == 1 = 2*(x*y+x*z+y*z)
              | otherwise = layer (n-1) + 4*(x+y+z) + 8*(n-2)

t x y z n = 2*(x*y+x*z+y*z) + 4*(x+y+z)*(n-1) + 4*(n-2)*(n-1)
s x y z n = 4*(x+y+z+n-2)*(n-1) + 2*x*y + 2*x*z + 2*y*z

triangle n = n*(n+1)/2
