import Data.Bits

type Fraction = (Integer,Integer)
type State = Integer
type Iteration = Integer
type Count = Integer

fractran :: State -> Iteration -> [Fraction] -> [(Iteration, Fraction, State)]
fractran seed iter program = exec seed program
    where exec state [] = [(state,(0,0),iter)]
          exec state ((n,d):fractions) =
              let (q,r) = quotRem (state * n) d
              in if r == 0
                 then (iter, (n,d), state) : fractran q (iter+1) program
                 else exec state fractions

isPowerOf2 :: State -> Bool
isPowerOf2 n = n == 1 || (n .&. (n-1)) == 0

process :: Count -> [(Iteration, Fraction, State)] -> [(Count, Iteration)]
process count [] = []
process count ((iter,_,state):rest) =
    if isPowerOf2 state
    then (count,iter):(process (count+1) rest)
    else process count rest


primeGame :: State -> Iteration -> [(Iteration, Fraction, State)]
primeGame state iteration
    | isPowerOf2 state =
        let n = truncate $ logBase 2.0 (fromIntegral state)
            (newState,newIteration) = rule2 n iteration
        in primeGame newState newIteration
    | otherwise = fractran state iteration conway

rule1 n i = ((3^n*5^(n+1)*11),(i+n+1))
rule2 n i = ((2*3*5^n*7^(n-1)*13),(3*n+4))
rule3 n i = ((2^(n+1)*3*7^(n-1)*13),(i+7*n+6))

test n = eq (takeWhile (hitIter n) $ primeGame 2 0)
            (takeWhile (hitIter n) $ fractran 2 0 conway)
    where eq [] _ = True
          eq (s:ss) answers = case dropWhile (/=s) answers of
              [] -> False
              (match:restAns) -> eq ss restAns
          hitIter x (i,_,s) = x == i

showResult (i,(n,d),s) =
    show i ++ ": " ++
    show s ++ " " ++
    show n ++ "/" ++ show d

conway = [
  (17,91), --7x13
  (78,85), --5x17
  (19,51), --3x17
  (23,38), --2x19
  (29,33), --3x11
  (77,29), --29
  (95,23), --23
  (77,29), --29
  (95,23), --23
  (77,19), --19
  ( 1,17), --17
  (11,13), --13
  (13,11), --11
  (15, 2), --2
  ( 1, 7), --7
  (55,1)]  --1

{-
Begin from a power of two, 2^N at iteration=0. The start state is expressed as:

0 : 2^N

The only fraction that whose denominator divides this number is 15/2, and this
will be true until you have divided away all the twos in the prime
factorization. Therefor we end up reaching this state:

N : 3^N * 5^N

There are no 15's in an denominators, leaving us with needing the 55/1 thus:

N+1 : 3^N * 5^(N+1) * 11

RULE #1: i:2^N -> (i+N+1):3^N * 5^(N+1) * 11

For as long as we have 3's in our prime factorization, we will hit 29/33,
remove the 11 and one 3 add a 29, then hit 77/29, remove the 29, add a 7 and
add back the 11. This is equivalent to the fraction 29/33*77/29 = 7/3.
This is two steps that will net us adding a 7, and removing a 3. Therefor
we will do this pair of 2 steps N times and end up with:

3N+1 : 5^(N+1) * 7^N * 11

The we hit 13/11 to give us:

3N+2 : 5^(N+1) * 7^N * 13

Then we match 17/91 = 17/(7*13) at get

3N+3 : 5^(N+1) * 7^(N-1) * 17

Then 78/85 to give us

3N+4 : 2 * 3 * 5^N * 7^(N-1) * 13

RULE #2: i:2^N -> (3N+4): 2 * 3 * 5^N * 7^(N-1) * 13

The it depends on what N is. If N=1, we have no 7's, so we'd match 11/13. If we
do, we'd match 17/91 = 17/(7*13) and then 78/85 = (2*3*13)/(5*17). This is a
two step process amounting to (2*3)/(5*7) giving us an addition 2,3 and
subtracting a 5 and a 7. This will repeat N-1 times, for a total of 2(N-1)
steps until we have no 7's in our prime factorization, leaving us with:

5N+2 : 2^N * 3^N * 5 * 13

We then match 11/13 to get

5N+3 : 2^N * 3^N * 5 * 11

As we saw in a previous steps, when we have 3^N and an 11 in our number, we
will match 29/33 and then 77/29 which amount to 7/3, turning all of our 3's
into 7's in 2N steps. Therefor:

7N+3 : 2^N * 5 * 7^N * 11

Then, again, the chain of 13/11, 17/91=17/(7*13), 78/85=(2*3*13)/(5*17) which
amounts to (2*3*13)/(5*7*11) in three steps. Leaving us with:

7N+6 : 2^(N+1) * 3 * 7^(N-1) * 13

-}
