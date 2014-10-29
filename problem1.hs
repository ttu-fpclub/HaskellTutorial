threes = [x | x <-[1..999], x `mod` 3 == 0, x `mod` 5 /= 0]
fives = [x | x <-[1..999], x `mod` 5 == 0]
total3 = sum threes
total5 = sum fives
total = total3 + total5
