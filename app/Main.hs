{-# language OverloadedStrings #-}
module Main where

import Buttplug.WebSockets
import Buttplug.ButtplugM
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent (threadDelay)

main :: IO ()
main = do
  let conn = Connector "127.0.0.1" 12345
  runButtplugWebSockets "testRequestDeviceList" conn $ do
    startScanning
    devices <- requestDeviceList
    liftIO $ print devices
