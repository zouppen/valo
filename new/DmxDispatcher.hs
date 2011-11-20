module DmxDispatcer where

import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Lazy

import Control.Concurrent.STM
import Control.Concurrent.STM.TChan


import EnttecDmxUsbPro
import Dmx


-- TODO add this check
--    | channel < 0 = fail "Channel below zero"
--    | channel >= universeSize = fail "Channel number too high"

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
