module DmxDispatcer where

import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Lazy

import Control.Concurrent.STM
import Control.Concurrent.STM.TChan


import EnttecDmxUsbPro
import Dmx



runReceiver devPath = do
  s <- create devPath
  runStateT run (s::Enttec)
  return ()
  where
--    run = reopenDevice >> forever handleCommand
    run = reopenDevice >> handleCommand >> closeDevice
    handleCommand = do
      setChannel 3 255
      setChannel 4 255
      sendFrame
