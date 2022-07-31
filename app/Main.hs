{-# language OverloadedStrings #-}
module Main where

import qualified Buttplug as BP
import qualified Buttplug.WebSockets as BPWS
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent (threadDelay)

main :: IO ()
main = do
  let conn = BPWS.Connector "127.0.0.1" 12345
  BPWS.runButtplugWebSockets "testRequestDeviceList" conn $ do
    BP.startScanning
    liftIO $ putStrLn "scanning... press enter to continue"
    _ <- liftIO getLine
    _ <- BP.requestDeviceList
    devices <- BP.getDevices
    liftIO $ do putStrLn "devices: "
                print devices
                putStrLn "press enter to continue: "
                _ <- getLine
                pure ()
    case devices of
      [d1] -> do 
        -- Should throw an exception if vibrator doesn't have motor at idx 1
        -- _ok <- vibrate d1 [Vibrate 6 0.1]
        _err <- BP.linearSingleActuator d1 300 0.5
        liftIO $ threadDelay 200000
        _ok <- BP.stopAllDevices
        pure ()
      [] -> do
        liftIO $ putStrLn "no devices connected"
        
