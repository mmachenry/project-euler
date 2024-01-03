ans :: Int -> Float -> Float
ans subject p = memoT subject
  where memoT :: Int -> Float
        memoT = (map t [0..] !!)
            where t :: Int -> Float
                  t 0 = 0
                  t 1 = 1
                  t s = 1.0 + (minimum $ map testGroups [1..s])
                      where testGroups n =
                                  (pInfected p n) * (memoD n)
                                + memoT (s-n)

        memoD :: Int -> Float
        memoD = (map d [0..] !!)
            where d :: Int -> Float
                  d 1 = 0
                  d s = 1.0 + (minimum $ map testGroups [1..s-1])
                      where testGroups n =
                                  (pInfected p n) * ((memoD n) + (memoT (s-n)))
                                + (pNotInfected p n) * (memoD (s-n))
    
pInfected p s = 1 - pNotInfected p s
pNotInfected p s = (1-p)^s

main = putStrLn $ show $ tests

tests = [
    (ans 25 0.02,4.155452)
  , (ans 25 0.10,12.702124)
  ]

t 1 p = 1
t 2 p =
    -- Test them both first
      1
    -- Next, if one is infected, then we test the first one.
    + (pInfected p 2) * (
          -- That's one more test plus another if it is infected
          1 + (pInfected p 1)*1 + (pNotInfected p 1)*0
      )
    -- If they are not infected we do nothing more.
    + (pNotInfected p 2) * (
          0
      )

