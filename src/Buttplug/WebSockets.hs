module Buttplug.WebSockets
  ( runButtplugWebSockets,
    WS.Connector(..),
  )
where

import Buttplug.ButtplugM as ButtplugM
import qualified Buttplug.Core.WebSockets as WS
import Data.Text (Text)

runButtplugWebSockets :: Text -> WS.Connector -> ButtplugM a -> IO a
runButtplugWebSockets clientName connector ma = do
  WS.runClient connector $ \handle -> do
    ButtplugM.runButtplug clientName handle $ ma
