{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Buttplug.Message
  ( Message (..),
    withMsgId,
    withoutMsgId,
    module Buttplug.Core.Message
  )
where

import Buttplug.Core.Device (Device)
import Buttplug.Core.Device qualified as Dev
import Buttplug.Core.Message
  ( ErrorCode,
    LinearActuate,
    RawData,
    Rotate,
    Vibrate,
    clientMessageVersion,
  )
import Buttplug.Core.Message qualified as CoreMsg
import Data.ByteString (ByteString)
import Data.Map (Map)
import Data.Text (Text)

data Message
  = -- status messages
    MsgOk
  | MsgError
      { msgErrorMessage :: Text,
        msgErrorCode :: ErrorCode
      }
  | MsgPing
  | -- handshake messages
    MsgRequestServerInfo
      { msgClientName :: Text,
        msgMessageVersion :: Word
      }
  | MsgServerInfo
      { msgServerName :: Text,
        msgMessageVersion :: Word,
        msgMaxPingTime :: Word
      }
  | -- enumeration messages
    MsgStartScanning
  | MsgStopScanning
  | MsgScanningFinished
  | MsgRequestDeviceList
  | MsgDeviceList
      { msgDevices :: [Device]
      }
  | MsgDeviceAdded
      { msgDeviceName :: Text,
        msgDeviceIndex :: Word,
        msgDeviceMessages :: Map Dev.DeviceMessageType Dev.MessageAttributes
      }
  | MsgDeviceRemoved
      { msgDeviceIndex :: Word
      }
  | -- raw device messages
    MsgRawWriteCmd
      { msgDeviceIndex :: Word,
        msgEndpoint :: Text,
        msgData :: RawData,
        msgWriteWithResponse :: Bool
      }
  | MsgRawReadCmd
      { msgDeviceIndex :: Word,
        msgEndpoint :: Text,
        msgExpectedLength :: Word,
        msgWaitForData :: Bool
      }
  | MsgRawReading
      { msgDeviceIndex :: Word,
        msgEndpoint :: Text,
        msgData :: RawData
      }
  | MsgRawSubscribeCmd
      { msgDeviceIndex :: Word,
        msgEndpoint :: Text
      }
  | MsgRawUnsubscribeCmd
      { msgDeviceIndex :: Word,
        msgEndpoint :: Text
      }
  | -- generic device messages
    MsgStopDeviceCmd
      { msgDeviceIndex :: Word
      }
  | MsgStopAllDevices
  | MsgVibrateCmd
      { msgDeviceIndex :: Word,
        msgSpeeds :: [Vibrate]
      }
  | MsgLinearCmd
      { msgDeviceIndex :: Word,
        msgVectors :: [LinearActuate]
      }
  | MsgRotateCmd
      { msgDeviceIndex :: Word,
        msgRotations :: [Rotate]
      }
  | -- generic sensor messages
    MsgBatteryLevelCmd
      { msgDeviceIndex :: Word
      }
  | MsgBatteryLevelReading
      { msgDeviceIndex :: Word,
        msgBatteryLevel :: Double
      }
  | MsgRSSILevelCmd
      { msgDeviceIndex :: Word
      }
  | MsgRSSILevelReading
      { msgDeviceIndex :: Word,
        msgRSSILevel :: Int
      }
  deriving (Show, Eq)

withMsgId :: Word -> Message -> CoreMsg.Message
withMsgId msgId = \case
  MsgOk -> CoreMsg.MsgOk {..}
  MsgError {..} -> CoreMsg.MsgError {..}
  MsgPing -> CoreMsg.MsgPing {..}
  -- handshake messages
  MsgRequestServerInfo {..} -> CoreMsg.MsgRequestServerInfo {..}
  MsgServerInfo {..} -> CoreMsg.MsgServerInfo {..}
  -- enumeration messages
  MsgStartScanning -> CoreMsg.MsgStartScanning {..}
  MsgStopScanning -> CoreMsg.MsgStopScanning {..}
  MsgScanningFinished -> CoreMsg.MsgScanningFinished {..}
  MsgRequestDeviceList -> CoreMsg.MsgRequestDeviceList {..}
  MsgDeviceList {..} -> CoreMsg.MsgDeviceList {..}
  MsgDeviceAdded {..} -> CoreMsg.MsgDeviceAdded {..}
  MsgDeviceRemoved {..} -> CoreMsg.MsgDeviceRemoved {..}
  -- raw device messages
  MsgRawWriteCmd {..} -> CoreMsg.MsgRawWriteCmd {..}
  MsgRawReadCmd {..} -> CoreMsg.MsgRawReadCmd {..}
  MsgRawReading {..} -> CoreMsg.MsgRawReading {..}
  MsgRawSubscribeCmd {..} -> CoreMsg.MsgRawSubscribeCmd {..}
  MsgRawUnsubscribeCmd {..} -> CoreMsg.MsgRawUnsubscribeCmd {..}
  -- generic device messages
  MsgStopDeviceCmd {..} -> CoreMsg.MsgStopDeviceCmd {..}
  MsgStopAllDevices -> CoreMsg.MsgStopAllDevices {..}
  MsgVibrateCmd {..} -> CoreMsg.MsgVibrateCmd {..}
  MsgLinearCmd {..} -> CoreMsg.MsgLinearCmd {..}
  MsgRotateCmd {..} -> CoreMsg.MsgRotateCmd {..}
  -- generic sensor messages
  MsgBatteryLevelCmd {..} -> CoreMsg.MsgBatteryLevelCmd {..}
  MsgBatteryLevelReading {..} -> CoreMsg.MsgBatteryLevelReading {..}
  MsgRSSILevelCmd {..} -> CoreMsg.MsgRSSILevelCmd {..}
  MsgRSSILevelReading {..} -> CoreMsg.MsgRSSILevelReading {..}

withoutMsgId :: CoreMsg.Message -> Message
withoutMsgId = \case
  CoreMsg.MsgOk {..} -> MsgOk 
  CoreMsg.MsgError {..} -> MsgError {..} 
  CoreMsg.MsgPing {..} -> MsgPing 
  -- handshake messages
  CoreMsg.MsgRequestServerInfo {..} -> MsgRequestServerInfo {..} 
  CoreMsg.MsgServerInfo {..} -> MsgServerInfo {..} 
  -- enumeration messages
  CoreMsg.MsgStartScanning {..} -> MsgStartScanning 
  CoreMsg.MsgStopScanning {..} -> MsgStopScanning 
  CoreMsg.MsgScanningFinished {..} -> MsgScanningFinished 
  CoreMsg.MsgRequestDeviceList {..} -> MsgRequestDeviceList 
  CoreMsg.MsgDeviceList {..} -> MsgDeviceList {..} 
  CoreMsg.MsgDeviceAdded {..} -> MsgDeviceAdded {..} 
  CoreMsg.MsgDeviceRemoved {..} -> MsgDeviceRemoved {..} 
  -- raw device messages
  CoreMsg.MsgRawWriteCmd {..} -> MsgRawWriteCmd {..} 
  CoreMsg.MsgRawReadCmd {..} -> MsgRawReadCmd {..} 
  CoreMsg.MsgRawReading {..} -> MsgRawReading {..} 
  CoreMsg.MsgRawSubscribeCmd {..} -> MsgRawSubscribeCmd {..} 
  CoreMsg.MsgRawUnsubscribeCmd {..} -> MsgRawUnsubscribeCmd {..} 
  -- generic device messages
  CoreMsg.MsgStopDeviceCmd {..} -> MsgStopDeviceCmd {..} 
  CoreMsg.MsgStopAllDevices {..} -> MsgStopAllDevices 
  CoreMsg.MsgVibrateCmd {..} -> MsgVibrateCmd {..} 
  CoreMsg.MsgLinearCmd {..} -> MsgLinearCmd {..} 
  CoreMsg.MsgRotateCmd {..} -> MsgRotateCmd {..} 
  -- generic sensor messages
  CoreMsg.MsgBatteryLevelCmd {..} -> MsgBatteryLevelCmd {..} 
  CoreMsg.MsgBatteryLevelReading {..} -> MsgBatteryLevelReading {..} 
  CoreMsg.MsgRSSILevelCmd {..} -> MsgRSSILevelCmd {..} 
  CoreMsg.MsgRSSILevelReading {..} -> MsgRSSILevelReading {..} 
