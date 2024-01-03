import Data.List

p m n = length $ nub $ sort [ m' * n' | m' <- [1..min m n], n' <- [m'..max m n]]
