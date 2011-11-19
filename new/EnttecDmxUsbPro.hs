-- |Simple module for controlling DMX. This doesn't
-- check all errors, like writing over headers with
-- negative addresses.
module EnttecDmxUsbPro where

import Control.Concurrent.STM
import Control.Concurrent.STM.TChan
import Control.Monad (unless)
import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Lazy
import Data.Array.IO
import Data.Word
import System.IO
import System.Exit
import System.Process (rawSystem)

type Control = StateT DmxData IO
type DmxArray = IOUArray Int Word8

data DmxData = DmxData { devH    :: Handle   -- ^Handle of Enttec.
                       , devPath :: FilePath -- ^Useful when reopening device.
                       , frame   :: DmxArray -- ^DMX frame.
                       }

-- Some constants.
universeSize = 512
channelZeroIx = length dmxHead -- Index of first DMX channel.
frameLength = length dmxHead + universeSize + 1
dmxHead = [0x7e,0x06,0x00,0x02] -- Some headers.
dmxTail = 0xe7 -- Last byte of a frame

-- |Opens DMX device initializes DMX data structure.  |in DMX data
-- structure. The universe is reset to zeros. It's up to upper level
-- to fill in some initial data.
initArray :: IO DmxArray
initArray = do
  arr <- newListArray (0,lastIndex) dmxHead  
  writeArray arr (lastIndex) dmxTail
  return arr
  where lastIndex = frameLength - 1

-- |Sets some serial port settings to avoid linefeed issue.
configureSerialPort :: FilePath -> IO ()
configureSerialPort devPath = do
  code <- rawSystem "stty" ["-F",devPath,"9600","cs8","-cstopb","-parodd","-parenb","raw"]
  unless (code==ExitSuccess) $ fail "Serial port configuration failed."

-- |Reopens a device. You are responsible of doing that only on a
-- closed handle or otherwise a resource exhaustion may occur.
reopenDevice :: Control ()
reopenDevice = do
  path <- gets devPath
  lift $ configureSerialPort path
  h <- lift $ openFile path WriteMode
  modify $ \x -> x{devH = h}

-- |Closes a device. Remember possible exceptions.
closeDevice :: Control ()
closeDevice = do
  h <- gets devH
  lift $ hClose h

-- |Sends a frame. You need to call this after you have modified frame
-- contents.
sendFrame :: Control ()
sendFrame = do
  h <- gets devH
  array <- gets frame
  lift $ hPutArray h array frameLength

-- |Sets DMX channel to a specified value
setChannel :: Int -> Word8 -> Control ()
setChannel channel value
  | channel < 0 = fail "Channel below zero"
  | channel >= universeSize = fail "Channel number too high"
  | otherwise = do
    array <- gets frame
    lift $ writeArray array (channelZeroIx + channel) value

