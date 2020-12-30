#!/usr/bin/env stack
-- stack --resolver lts-12.21 script

-- EXERCISE: Define a helper function of type String -> RIO App () and use it in sayHello
-- and sayGoodbye instead of calling hPutStrLn directly.

{-# LANGUAGE NoImplicitPrelude #-}

import RIO
import System.IO (hPutStrLn, stderr)

data App = App
  { appName :: !String
  , appHandle :: !Handle
  }

main :: IO ()
main = do
  let app = App
        { appName = "Alice"
        , appHandle = stderr
        }
  runRIO app $ do
    sayHello
    sayGoodbye

sayHello :: RIO App ()
sayHello = do
  App name h <- ask
  say $ "Hello, " ++ name

sayGoodbye :: RIO App ()
sayGoodbye = do
  App name h <- ask
  say $ "Goodbye, " ++ name

say :: String -> RIO App ()
say s = do
  App _n h <- ask
  liftIO $ hPutStrLn h s
