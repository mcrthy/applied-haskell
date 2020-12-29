#!/usr/bin/env stack
-- stack --resolver lts-12.21 script

-- EXERCISE: The following program doesn't force any evaluation, because GHC only cares
-- about performing the IO actions it is told about in the main function. In order to
-- force greater usage of memory:
-- 1) add a bang pattern somewhere in main.
-- 2) add a seq somewhere in the last line of main.

{-# LANGUAGE BangPatterns #-}

data StrictList a = Cons !a !(StrictList a) | Nil

strictMap :: (a -> b) -> StrictList a -> StrictList b
strictMap _ Nil = Nil
strictMap f (Cons a list) =
  let !b = f a
      !list' = strictMap f list
   in b `seq` list' `seq` Cons b list'

strictEnum :: Int -> Int -> StrictList Int
strictEnum low high =
  go low
  where
    go !x
      | x == high = Cons x Nil
      | otherwise = Cons x (go $! x + 1)
 
double :: Int -> Int
double !x = x * 2

evens :: StrictList Int
evens = strictMap double $! strictEnum 1 1000000

main :: IO ()
main = do
  let string = "Hello World"
      !string' = evens `seq` string -- this forces the evaluation of string' regardless if it is used in IO
  string' `seq` putStrLn string -- this forces the evaluation of string' before "Hello World" is printed



