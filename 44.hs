p n = n * (3*n - 1) / 2

pentagonalIndex p = properFraction ((sqrt(1+24*p) + 1) / 6)

find :: Integer -> Integer -> Integer
find d_i low_p_i =
    let low_p = p low_p_i
        d = p d_i
        (i,dec) = pentagonalIndex (fromIntegral (low_p + d))
    in if dec == 0.0
       then d_i
       else if d < i
            then find (d_i+1) 1
            else find d_i (low_p_i+1)

main = putStrLn $ show (find 1 1)
