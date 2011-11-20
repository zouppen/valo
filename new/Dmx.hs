-- |Common operations for a DMX512 bus.
module Dmx where

import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Lazy
import Data.Word

universeSize :: Int
universeSize = 512

type Dmx s = StateT s IO

-- |Typeclass which contains common operations for all DMX devices.
class DmxHost t where
  -- |Creates DMX device and returns it
  create       :: FilePath -> IO t
  
  -- |Reopens a device. You are responsible of doing that only on a
  -- closed handle or otherwise a resource exhaustion may occur.
  reopenDevice :: Dmx t ()
  
  -- |Closes a device. Remember to handle possible exceptions.
  closeDevice  :: Dmx t ()
  
  -- |Sets DMX channel to a specified value.
  setChannel   :: Int -> Word8 -> Dmx t ()
  
  -- |Sends a frame. You need to call this after you have modified
  -- frame contents.
  sendFrame    :: Dmx t () 
