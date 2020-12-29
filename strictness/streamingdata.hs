-- Using the Conduit streaming data library, write list averaging functions that run in constant memory
-- by utilizing:
-- 1) the 'force' function
-- 2) bang patterns
-- 3) a custom data type with strict fields

{-# LANGUAGE BangPatterns #-}

import Conduit
import Control.DeepSeq (force)

average :: Monad m => ConduitM Int o m Double
average =
  divide <$> foldlC add (0, 0)
  where
    divide (total, count) = fromIntegral total / count
    add (total, count) x = force (total + x, count + 1)


average' :: Monad m => ConduitM Int o m Double
average' =
  divide <$> foldlC add (0, 0)
  where
    divide (!total, !count) = fromIntegral total / count
    add (!total, !count) x = (total + x, count + 1)

data RunningTotal =
  RunningTotal
  {
    sum   :: !Int
  , count :: !Int
  }

average'' :: Monad m => ConduitM Int o m Double
average'' =
  divide <$> foldlC add (RunningTotal 0 0)
  where
    divide (RunningTotal total count) = fromIntegral total / fromIntegral count :: Double
    add (RunningTotal total count) x = RunningTotal (total + x) (count + 1)
 
main :: IO ()
main = do
  let enumList = enumFromToC 1 1000000
  print $ runConduitPure $ enumList .| average
  print $ runConduitPure $ enumList .| average'
  print $ runConduitPure $ enumList .| average''
