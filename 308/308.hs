solve num = outter 1 1 0 0
    where outter n d pcount steps =
              if pcount < num
              then inner2 (n+1) n pcount ((steps+1)+3*(n+1)+d-2)
              else steps

          inner2 n d pcount steps =
              let (q,r) = quotRem n 2
              in if r == 0 && n /= 2
                 then outter n q pcount
                             (steps+4*n+2*2+1+(n-q-1)*(6*n+2+2))
                 else inner n d pcount steps

          inner n d pcount steps =
              let (q,r) = quotRem n d
              in if r == 0
                 then outter n d
                             (pcount + (if d == 1 then 1 else 0))
                             (steps+4*n+2*q+1)
                 else inner n (d-1) pcount (steps+6*n+2*q+2)

main = putStrLn $ show $ solve 1000

