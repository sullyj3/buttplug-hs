{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeFamilies #-}

module Buttplug.ButtplugM
  ( ButtplugM,
    requestDeviceList,
    getDevices,
    runButtplug,
    sendMessages,
    sendMessage,
    startScanning,
    vibrate,
    vibrateSingleMotor,
    linear,
    linearSingleActuator,
    stopAllDevices,
    UnexpectedResponse(..),
    addDeviceConnectedHandler
  )
where

-- start with a concrete monad, we'll figure out how to do a transformer later

import Buttplug.Core.Handle (ButtplugException (OtherConnectorError), Handle)
import Buttplug.Core.Handle qualified as Handle
import Buttplug.Core.Message qualified as CoreMsg
import Buttplug.Message
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar (newTVarIO)
import Control.Exception (Exception, throwIO)
import Control.Monad (forever)
import Control.Monad.Base
import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class
import Control.Monad.IO.Unlift
import Control.Monad.Reader.Class
import Control.Monad.Trans.Control
import Control.Monad.Trans.Reader (ReaderT, runReaderT)
import Data.Foldable
import Data.Text (Text)
import StmContainers.Map qualified as STMMap
import Control.Concurrent.MVar
import Ki.Unlifted
import Control.Concurrent (threadDelay)
import Data.Functor (($>))
import UnliftIO.Exception qualified as Exc

data ServerInfo = ServerInfo
  { serverName :: Text,
    serverMsgVersion :: Word,
    serverMaxPingtime :: Word
  }

data BPEnv = BPEnv
  { clientName :: Text,
    handle :: Handle,
    serverInfo :: ServerInfo,
    msgIdCounter :: TVar Word,
    devices :: TMVar [Device],
    pendingResponses :: STMMap.Map Word (Message -> ButtplugM ()),
    deviceConnectedHandlers :: TVar [Device -> ButtplugM ()],
    userThreads :: Scope
  }

addDeviceConnectedHandler :: (Device -> ButtplugM ()) -> ButtplugM ()
addDeviceConnectedHandler k = do
  BPEnv { deviceConnectedHandlers } <- ask
  liftIO . atomically $ modifyTVar' deviceConnectedHandlers (k:)

vibrate :: Device -> [Vibrate] -> ButtplugM Message
vibrate (Device {deviceIndex}) speeds = do
  sendMessageExpectOk (MsgVibrateCmd deviceIndex speeds)

linear :: Device -> [LinearActuate] -> ButtplugM Message
linear (Device {deviceIndex}) linActuates = do
  sendMessageExpectOk (MsgLinearCmd deviceIndex linActuates)

linearSingleActuator :: Device -> Word -> Double -> ButtplugM Message
linearSingleActuator device duration position =
  linear device [LinearActuate 0 duration position]

vibrateSingleMotor device speed = vibrate device [Vibrate 0 speed]

stopAllDevices = sendMessageExpectOk MsgStopAllDevices

-- expect response

newtype UnexpectedResponse = UnexpectedResponse Message
  deriving (Show)

instance Exception UnexpectedResponse

-- We're expecting a response from the server with a particular message id
-- f encodes our expectations about the response, returning Nothing if it's unexpected
-- when we get the response, apply f to it, returning the result if it 
-- succeeds, or throwing UnexpectedResponse if it fails
expectResponse :: (Message -> Maybe b) -> Word -> ButtplugM b
expectResponse f msgId = do
  -- `pending` maps pending response message ids to callbacks that should be performed 
  -- on those responses. Calling the callback is performed by handleIncomingMessages
  pending <- asks pendingResponses
  output <- liftIO newEmptyMVar
  let writeOutput msg = liftIO $ putMVar output result
        where
          result = case f msg of
            Just x -> Right x
            Nothing -> Left $ UnexpectedResponse msg
  liftIO $ do
    atomically $ STMMap.insert writeOutput msgId pending
    takeMVar output >>= Exc.fromEither 

-- throws UnexpectedResponse
sendMessageExpectResponse ::
  (Message -> Maybe a) -> Message -> ButtplugM a
sendMessageExpectResponse expect msg = do
  msgId <- nextMsgId
  let coreMsg = withMsgId msgId msg
  sendCoreMessage coreMsg
  expectResponse expect msgId

-- throws UnexpectedResponse
sendMessageExpectOk :: Message -> ButtplugM Message
sendMessageExpectOk = sendMessageExpectResponse $ \msg -> case msg of
  MsgOk -> Just msg
  _ -> Nothing

requestDeviceList :: ButtplugM [Device]
requestDeviceList = sendMessageExpectResponse
  (\case MsgDeviceList devList -> Just devList
         _ -> Nothing)
  MsgRequestDeviceList

startScanning :: ButtplugM Message
startScanning = sendMessageExpectOk MsgStartScanning


-- Post-increment
nextMsgId = do
  msgIdVar <- asks msgIdCounter
  liftIO $ atomically $ stateTVar msgIdVar $ \n -> (n, n + 1)

isOk = \case
  MsgOk -> True
  _ -> False

newtype ButtplugM a = ButtplugM {runButtplugM :: ReaderT BPEnv IO a}
  deriving newtype
    ( Functor,
      Applicative,
      Monad,
      MonadFail,
      MonadThrow,
      MonadIO,
      MonadReader BPEnv,
      MonadUnliftIO,
      MonadBase IO
    )

instance MonadBaseControl IO ButtplugM where
  type StM ButtplugM a = a
  liftBaseWith f = ButtplugM $ liftBaseWith $ \q -> f (q . runButtplugM)
  restoreM = ButtplugM . restoreM
runButtplug :: Text -> Handle -> ButtplugM a -> IO a
runButtplug clientName handle bp = do
  serverInfo <- handshake clientName handle
  msgIdCounter <- newTVarIO 2
  devices <- newEmptyTMVarIO
  pendingResponses <- STMMap.newIO
  deviceConnectedHandlers <- newTVarIO []
  scoped $ \userThreads -> do
    let env =
          BPEnv
            clientName
            handle
            serverInfo
            msgIdCounter
            devices
            pendingResponses
            deviceConnectedHandlers
            userThreads
    flip runReaderT env . runButtplugM $
      scoped $ \scope -> do
        _ <- fork scope $ forever handleIncomingMessages
        requestDeviceList
        bp
  where
    handshake :: Text -> Handle -> IO ServerInfo
    handshake clientName handle = do
      Handle.sendMessage handle $ CoreMsg.MsgRequestServerInfo 1 clientName clientMessageVersion
      resp <- Handle.receiveMessages handle
      case resp of
        [ CoreMsg.MsgServerInfo
            1
            msgServerName
            msgMessageVersion
            msgMaxPingTime
          ] ->
            pure $
              ServerInfo
                { serverName = msgServerName,
                  serverMsgVersion = msgMessageVersion,
                  serverMaxPingtime = msgMaxPingTime
                }
        msgs ->
          throwIO . OtherConnectorError $
            "Server sent unexpected response to handshake: " <> show msgs


-- `retry` until it exists
getDevices :: ButtplugM [Device]
getDevices = do
  devicesVar <- asks devices
  liftIO . atomically $ readTMVar devicesVar

sendMessages :: [Message] -> ButtplugM ()
sendMessages msgs = do
  msgIdVar <- asks msgIdCounter
  msgId <- liftIO $
    atomically $
      stateTVar msgIdVar $
        \n -> (n, n + fromIntegral (length msgs))
  let coreMsgs = zipWith withMsgId [msgId ..] msgs
  sendCoreMessages coreMsgs
  where
    sendCoreMessages :: [CoreMsg.Message] -> ButtplugM ()
    sendCoreMessages msgs = withHandle $ \h -> Handle.sendMessages h msgs

sendMessage :: Message -> ButtplugM ()
sendMessage msg = do
  msgId <- nextMsgId
  sendCoreMessage $ withMsgId msgId msg

sendCoreMessage :: CoreMsg.Message -> ButtplugM ()
sendCoreMessage msg = do
  withHandle $ \h -> Handle.sendMessage h msg

handleIncomingMessages :: ButtplugM ()
handleIncomingMessages =
  traverse_ handleIncomingMessage =<< receiveCoreMessages
  where
    receiveCoreMessages :: ButtplugM [CoreMsg.Message]
    receiveCoreMessages = withHandle Handle.receiveMessages

    handleIncomingMessage :: CoreMsg.Message -> ButtplugM ()
    handleIncomingMessage msg = do
      handlePendingResponse msg
      case withoutMsgId msg of
        MsgDeviceList devList -> updateDevices devList
        MsgDeviceAdded name index messages -> 
        -- todo: also update device list
          let dev = Device name index messages
          in  runDeviceAddedHandlers dev
        _ -> pure ()

    handlePendingResponse :: CoreMsg.Message -> ButtplugM ()
    handlePendingResponse msg = do
      pending <- asks pendingResponses
      mCallback <- liftIO . atomically $ STMMap.lookup (CoreMsg.msgId msg) pending
      case mCallback of
        Just callback -> callback (withoutMsgId msg)
        Nothing -> pure ()

    updateDevices :: [Device] -> ButtplugM ()
    updateDevices devList = do
      devicesVar <- asks devices
      liftIO . atomically $ do
        isEmptyTMVar devicesVar >>= \case
          True -> putTMVar devicesVar devList
          False -> swapTMVar devicesVar devList $> ()

    runDeviceAddedHandlers dev = do
      BPEnv { deviceConnectedHandlers, userThreads } <- ask
      handlers <- liftIO $ readTVarIO deviceConnectedHandlers
      traverse_ (fork userThreads . ($ dev)) handlers

-- Continuation must return to ensure we put the handle back for other threads
withHandle :: (Handle -> IO a) -> ButtplugM a
withHandle k = do
  hndl <- asks handle
  liftIO $ k hndl
