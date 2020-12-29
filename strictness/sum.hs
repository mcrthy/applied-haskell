-- EXERCISE: Write list summation functions using:
-- 1) the bang pattern
-- 2) the seq function 
-- to force the summation of the total before each recursion.

{-# LANGUAGE BangPatterns #-}

sum' :: [Int] -> Int
sum' xs =
  go xs 0
  where
    go [] !total = total
    go (x:xs) !total = go xs (total + x)

sum'' :: [Int] -> Int
sum'' xs =
  go xs 0
  where
    go [] total = total
    go (x:xs) total =
      let t = total + x
      in t `seq` go xs t 
