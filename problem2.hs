fib :: Integer -> Integer
fib 1 = 1
fib 0 = 0
fib x = fib (x-1) + fib (x-2)


solveIt :: Integer -> Integer -> Integer -> Integer
solveIt previous current total | previous + current < 4000000  && ((previous+current) `mod` 2 == 0) = solveIt current (previous+current) (total + previous + current)
                               | previous + current < 4000000 = solveIt current (previous+current) total
                               | otherwise = total

