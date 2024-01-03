base :: Integer
base = 7^9
--m x = (2^x + 1) ^2 - (3^x + 1)
part1 n = ( 3*n + 3 * 2^(n+2) + 2^(2*n+2) - 4) `quot` 3
part2 n = ( 2*n + 3^(n+1) + 1 ) `quot` 2
solve x = ( (part1 x) - (part2 x) ) `mod` base
solution = solve (truncate 1e18)
