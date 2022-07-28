{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module Buttplug.ButtplugM
  ( ButtplugM,
    runButtplug,
    sendMessages,
    sendMessage,
    receiveMessages
  )
where

-- start with a concrete monad, we'll figure out how to do a transformer later

import Buttplug.Core.Handle (ButtplugException (OtherConnectorError), Handle)
import Buttplug.Core.Handle qualified as Handle
import Buttplug.Core.Message qualified as CoreMsg
import Buttplug.Message
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar (newTVarIO)
import Control.Exception (throwIO)
import Control.Monad.Base
import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class
import Control.Monad.IO.Unlift
import Control.Monad.Reader.Class
import Control.Monad.Trans.Control
import Control.Monad.Trans.Reader (ReaderT, runReaderT)
import Data.Text (Text)

-- TODO
-- We should maintain a list of available devices, and allow users to request them directly

data ServerInfo = ServerInfo
  { serverName :: Text,
    serverMsgVersion :: Word,
    serverMaxPingtime :: Word
  }

data BPEnv = BPEnv
  { clientName :: Text,
    handle :: Handle,
    serverInfo :: ServerInfo,
    msgIdCounter :: TVar Word
  }

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

runButtplug :: Text -> Handle -> ButtplugM a -> IO a
runButtplug clientName handle bp = do
  serverInfo <- handshake clientName handle
  msgIdCounter <- newTVarIO 2
  let env = BPEnv clientName handle serverInfo msgIdCounter
  flip runReaderT env . runButtplugM $ bp

sendCoreMessages :: [CoreMsg.Message] -> ButtplugM ()
sendCoreMessages msgs = withHandle $ \h -> Handle.sendMessages h msgs

sendCoreMessage :: CoreMsg.Message -> ButtplugM ()
sendCoreMessage msg = withHandle $ \h -> Handle.sendMessage h msg

sendMessages :: [Message] -> ButtplugM ()
sendMessages msgs = do
  msgIdVar <- asks msgIdCounter
  msgId <- liftIO $
    atomically $
      stateTVar msgIdVar $
        \n -> (n, n + fromIntegral (length msgs))
  let coreMsgs = zipWith withMsgId [msgId ..] msgs
  sendCoreMessages coreMsgs

sendMessage :: Message -> ButtplugM ()
sendMessage msg = do
  msgIdVar <- asks msgIdCounter
  msgId <- liftIO $ atomically $ stateTVar msgIdVar $ \n -> (n, n + 1)
  sendCoreMessage $ withMsgId msgId msg

receiveCoreMessages :: ButtplugM [CoreMsg.Message]
receiveCoreMessages = withHandle Handle.receiveMessages

receiveMessages :: ButtplugM [Message]
receiveMessages = map withoutMsgId <$> receiveCoreMessages

withHandle :: (Handle -> IO a) -> ButtplugM a
withHandle k = liftIO . k =<< asks handle
