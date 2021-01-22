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

data RunningAverage =
  RunningAverage
  {
    sum   :: !Int
  , count :: !Int
  } deriving (Eq, Show)

genRunningAverage :: Gen (RunningAverage)
genRunningAverage = do
  s <- arbitrary
  c <- arbitrary
  return $ RunningAverage s c

instance Arbitrary RunningAverage where
  arbitrary = genRunningAverage

instance Semigroup RunningAverage where
  (<>) (RunningAverage s c) (RunningAverage s' c') =
    RunningAverage (s + s') (c + c')

instance Monoid RunningAverage where
  mempty = RunningAverage 0 0

instance EqProp RunningAverage where
  (=-=) = eq

average :: RunningAverage -> Double
average (RunningAverage s c) = fromIntegral s / fromIntegral c

main :: IO ()
main = do
  let rt = RunningAverage 10 7
  
  print $ average rt
  quickBatch $ monoid rt

