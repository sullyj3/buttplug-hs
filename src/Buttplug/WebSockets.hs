module Buttplug.WebSockets
  ( runButtplugWebSockets,
    WS.Connector(..),
  )
where

import Buttplug.ButtplugM as ButtplugM
import qualified Buttplug.Core.WebSockets as WS

runButtplugWebSockets :: WS.Connector -> ButtplugM a -> IO a
runButtplugWebSockets connector ma = do
  WS.runClient connector $ \handle -> do
    ButtplugM.runButtplug handle $ ma
