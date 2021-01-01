#!/usr/bin/env stack
-- stack --resolver lts-12.21 script

-- EXERCISE: Define a new data type, App2, such that the following code works:
--
--    let app2 = App2 {app2Handle = stdout, app2FavoriteColor = "blue" }
--    runRIO app2 saytime

-- Also, write a sayFavoriteColor action.

{-# LANGUAGE NoImplicitPrelude #-}

import RIO
import RIO.Time (getCurrentTime)
import System.IO (hPutStrLn, stderr)

data App = App
  { appName :: !String
  , appHandle :: !Handle
  }

data App2 = App2
  { app2FavoriteColor :: !String
  , app2Handle :: !Handle
  }

class HasHandle env where
  getHandle :: env -> Handle

instance HasHandle Handle where
  getHandle = id

instance HasHandle App where
  getHandle = appHandle

instance HasHandle App2 where
  getHandle = app2Handle

main :: IO ()
main = do
  let app = App
        { appName = "Alice"
        , appHandle = stdout
        }

  runRIO app $ do
    sayHello
    sayTime
    sayGoodbye

  let app2 = App2
        { app2FavoriteColor = "blue"
        , app2Handle = stdout
        }

  runRIO app2 $ do
    sayFavoriteColor
  
  runRIO app2 sayTime

say :: HasHandle env => String -> RIO env ()
say msg = do
  env <- ask
  liftIO $ hPutStrLn (getHandle env) msg

sayTime :: HasHandle env => RIO env ()
sayTime = do
  now <- getCurrentTime
  say $ "The time is: " ++ show now

sayHello :: RIO App ()
sayHello = do
  App name _h <- ask
  say $ "Hello, " ++ name

sayGoodbye :: RIO App ()
sayGoodbye = do
  App name _h <- ask
  say $ "Goodbye, " ++ name

sayFavoriteColor :: RIO App2 ()
sayFavoriteColor = do
  App2 color _h <- ask
  say $ "My favourite colour is " ++ color
