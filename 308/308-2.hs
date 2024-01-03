main = putStrLn $ show $ solve 1000

solve num = outer num 0 0 1 1

outer num pcount steps initN d =
    if pcount < num
    then let n = initN+1
         in inner num pcount (steps+3*n+d-2+1) n (n-1) 1
   else steps

inner num pcount steps n initHigh initP =
    let p = if initHigh == 1 then n else initP+1 in
    let (initLow,r) = quotRem n p in
    let low = if r == 0 then initLow else initLow+1 in
    let runs = initHigh-low in
    let high = low-1 in
        if r == 0
        then outer num
                   (pcount+(if p==n then 1 else 0))
                   (steps+(4*n+2*p+1+(6*n+2*(p-1)+2)*runs))
                   n
                   low
        else inner num pcount (steps+(6*n+2*(p-1)+2)*(runs+1)) n high p
