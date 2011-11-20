-- |Simple module for controlling DMX. This doesn't
-- check all errors, like writing over headers with
-- negative addresses.
module EnttecDmxUsbPro (Enttec) where

import Control.Monad (unless)
import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Lazy
import Data.Array.IO
import Data.Word
import System.IO
import System.Exit
import System.Process (rawSystem)

import Dmx

type FrameArray = IOUArray Int Word8

data Enttec = Enttec { devH    :: Handle     -- ^Handle of Enttec.
                     , devPath :: FilePath   -- ^Useful when reopening device.
                     , frame   :: FrameArray -- ^DMX frame.
                     }

-- Some constants.
channelZeroIx = 4 -- Index of first DMX channel.
frameLength = universeSize + 5

-- |Opens DMX device initializes DMX data structure.  |in DMX data
-- structure. The universe is reset to zeros. It's up to upper level
-- to fill in some initial data.
initArray :: IO FrameArray
initArray = do
  -- Every channel is 0 by default.
  arr <- newArray (0,frameLength-1) 0
  
  -- Fill in constants.
  writeArray arr 0 0x7e
  writeArray arr 1 0x06
  writeArray arr 2 0x00
  writeArray arr 3 0x02
  writeArray arr (frameLength-1) 0xe7
  
  return arr

-- |Sets some serial port settings to avoid linefeed issue.
configureSerialPort :: FilePath -> IO ()
configureSerialPort devPath = do
  code <- rawSystem "stty" ["-F",devPath,"9600","cs8","-cstopb","-parodd","-parenb","raw"]
  unless (code==ExitSuccess) $ fail "Serial port configuration failed."

instance DmxHost Enttec where
  create devPath = do
    frame <- initArray
    return $ Enttec { devH    = undefined
                    , devPath = devPath
                    , frame   = frame
                    }
  
  reopenDevice = do
    path <- gets devPath
    lift $ configureSerialPort path
    h <- lift $ openFile path WriteMode
    modify $ \x -> x{devH = h}

  closeDevice = do
    h <- gets devH
    lift $ hClose h
  
  setChannel channel value = do
    array <- gets frame
    lift $ writeArray array (channelZeroIx + channel) value

  sendFrame = do
    h <- gets devH
    array <- gets frame
    lift $ hPutArray h array frameLength
