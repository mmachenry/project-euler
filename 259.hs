numCat x y = x * ceiling(logBase 10 y) + y
operators = [+ - * / numCat]

sumOfExpressions [] exp _ _ = eval exp
sumOfExpressions nums exp numbersInExp numCatable
    | numbersInExp < 2 =
        sumOfExpressions numbers (n:exp) (numbersInExp+1) (numCatable+1)
    | otherwise =
        sum $ map (/op->sumOfExpressions nums (op:exp) numbersInExp numCatable)
                  (if numCatable < 2 then operators else numCat:operators)

solution :: Integer
solution [1..9] [] 0 0

