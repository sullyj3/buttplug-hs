{-# language OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
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
        result <- vibrateSingleMotor d1 0.1
        case result of
          Left (UnexpectedResponse errmsg) -> liftIO $ print errmsg
          Right _ok -> do
            liftIO $ threadDelay 200000
            stopAllDevices
            pure ()
      [] -> do
        liftIO $ putStrLn "no devices connected"
        
