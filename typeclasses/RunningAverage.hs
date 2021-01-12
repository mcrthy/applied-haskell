#!/usr/bin/env stack
-- stack --resolver lts-12.21 script

-- EXERCISE: Write a data type for calculating the average of
-- a bunch of integers. Write Semigroup and Monoid instances for it,
-- and test them. Define an average function that calculates the
-- average from these two fields.

{-# LANGUAGE BangPatterns #-}

import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data RunningTotal =
  RunningTotal
  {
    sum   :: !Int
  , count :: !Int
  } deriving (Eq, Show)

instance Arbitrary RunningTotal where
  arbitrary = do
    s <- arbitrary
    c <- arbitrary
    return (RunningTotal s c)

instance Semigroup RunningTotal where
  (<>) (RunningTotal s c) (RunningTotal s' c') =
    RunningTotal (s + s') (c + c')

instance Monoid RunningTotal where
  mempty = RunningTotal 0 0

instance EqProp RunningTotal where
  (=-=) = eq

average :: RunningTotal -> Double
average (RunningTotal s c) = fromIntegral s / fromIntegral c

main :: IO ()
main = do
  let rt = RunningTotal 10 7
  
  print $ average rt
  quickBatch $ monoid $ rt

