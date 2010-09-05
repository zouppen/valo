-- |Simple module for controlling DMX. This doesn't
-- check all errors, like writing over headers with
-- negative addresses.

module DMX where

import Data.Word
import System.IO
import Data.Array.IO

data Bus = Bus {
      devH   :: Handle
    , states :: IOUArray Int Word8
}

busLength = 512 + 5 -- Hard-coded 512-length bus plus header.
label     = 6       -- Mystical constant.
firstChan = 4       -- Byte offset of first channel.

createBus :: FilePath -> IO Bus
createBus device = do
  h <- openFile device WriteMode
  arr <- newArray (0,busLength) 0
  
  -- Setting static values for DMX bus.
  writeArray arr 0 0x7e
  writeArray arr 1 label
  writeArray arr 3 0x02 -- Data length: 512
 
  return $ Bus h arr

closeBus :: Bus -> IO ()
closeBus bus = do
  hClose $ devH bus

sendDMX :: Bus -> IO ()
sendDMX bus = do
  hPutArray (devH bus) (states bus) busLength

--setChannel :: (MArray IOUArray Word8 m) => Bus -> Int -> Word8 -> m ()
setChannel bus channel value = do
  writeArray (states bus) (channel+firstChan) value
