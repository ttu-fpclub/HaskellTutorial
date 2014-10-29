-- This is the basic fibonacci sequence, however we do not want to use this.
fib :: Integer -> Integer
fib 1 = 1
fib 0 = 0
fib x = fib (x-1) + fib (x-2)
-- Denote that that form is fib(n) = fib(n-1) + fib(n-2) 
-- if you know fib(n-1) and fib(n-2, then you can find fib(n)
-- because we only want even I use a guard with three blocks to have one for simply continuing the squence, one to continue and add to the total, and one to stop.

solveIt :: Integer -> Integer -> Integer -> Integer
solveIt previous current total | previous + current < 4000000  && ((previous+current) `mod` 2 == 0) = solveIt current (previous+current) (total + previous + current)
                               | previous + current < 4000000 = solveIt current (previous+current) total
                               | otherwise = total

