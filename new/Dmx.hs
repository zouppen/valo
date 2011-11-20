-- |Common operations for a DMX512 bus.
module Dmx where

import Control.Concurrent (forkIO)
import Control.Concurrent.STM
import Control.Monad (forever)
import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Lazy
import Data.Word

universeSize :: Int
universeSize = 512

type Dmx s = StateT s IO

data DmxCommand = SetChannel Int Word8 | Flush

-- |Typeclass which contains common operations for all DMX devices.
class DmxHost t where
  -- |Opens a device. You are responsible of doing that only on a
  -- closed handle or otherwise a resource exhaustion may occur.
  openDevice :: Dmx t ()
  
  -- |Closes a device. Remember to handle possible exceptions.
  closeDevice  :: Dmx t ()
  
  -- |Sets DMX channel to a specified value. Does not need to contain
  -- array boundary checks.
  setChannel'   :: Int -> Word8 -> Dmx t ()
  
  -- |Sends a frame. You need to call this after you have modified
  -- frame contents.
  sendFrame    :: Dmx t () 

-- |Sets DMX channel to a specific value. Checks error conditions.
setChannel :: (DmxHost t) => Int -> Word8 -> Dmx t ()
setChannel ch | ch < 0 = fail "Channel below zero"
              | ch >= universeSize = fail "Channel number too high"
              | otherwise = setChannel' ch

-- |Runs DMX handler thread
--runDmxListener :: (DmxHost t) => t -> IO  = do
runDmxListerer t = do  
  chan <- newTChanIO
  forkIO $ runStateT (start chan) t >> return ()
  return chan
  where
    start chan = do
      openDevice
      forever $ work chan
    work chan = do
      e <- lift $ atomically $ readTChan chan
      case e of
        SetChannel ch a -> setChannel ch a
        Flush -> sendFrame
      

-- DMX lights. TODO move to a new module if needed.
--data Light = Light {
--      address :: Int
--    , bus     :: Bus
--}
