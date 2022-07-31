{-# language OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Main where

import Buttplug.WebSockets
import Buttplug.ButtplugM
import Buttplug.Message (Vibrate(..))
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent (threadDelay)

main :: IO ()
main = do
  let conn = Connector "127.0.0.1" 12345
  runButtplugWebSockets "testRequestDeviceList" conn $ do
    startScanning
    liftIO $ putStrLn "scanning... press enter to continue"
    _ <- liftIO getLine
    _ <- requestDeviceList
    devices <- getDevices
    liftIO $ do putStrLn "devices: "
                print devices
                putStrLn "press enter to continue: "
                _ <- getLine
                pure ()
    case devices of
      [d1] -> do 
        -- Should throw an exception if vibrator doesn't have motor at idx 1
        -- _ok <- vibrate d1 [Vibrate 6 0.1]
        _err <- linearSingleActuator d1 300 0.5
        liftIO $ threadDelay 200000
        _ok <- stopAllDevices
        pure ()
      [] -> do
        liftIO $ putStrLn "no devices connected"
        
