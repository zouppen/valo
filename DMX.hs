-- |Simple module for controlling DMX. This doesn't
-- check all errors, like writing over headers with
-- negative addresses.

module DMX where

import Data.Bits
import Data.Word
import System.IO
import Data.Array.IO

data Bus = Bus {
      devH   :: Handle
    , states :: IOUArray Int Word8
}

busChans   = 512 :: Int   -- Channels on bus.
dataLength = busChans + 5 -- Header included.
label      = 6            -- Mystical constant.
firstChan  = 4            -- Byte offset of first channel.

-- |Opens DMX device initializes DMX data structure.  |in DMX data
-- structure. Initial table is given as a plain 8-bit array.
--createBus :: FilePath -> [Word8] -> IO Bus
createBus device initial = do
  -- Opens DMX device.
  h <- openFile device WriteMode

  -- Every channel is 0 by default.
  arr <- newArray (0,dataLength-1) 0
  
  -- First 2 bytes.
  writeArray arr 0 0x7e
  writeArray arr 1 label

  -- Write bus channel count in little-endian format. Array contains
  -- Word8 elements, therefore .&. 0xff is done automatically.
  writeArray arr 2 $ fromIntegral busChans
  writeArray arr 3 $ fromIntegral (shiftR busChans 8)

  -- Writes terminating byte.
  writeArray arr (dataLength-1) 0xe7

  -- Fill in initial values.
  let bus = Bus h arr
  mapM_ (uncurry $ setChannel bus) initial
      
  -- Send initial content
  sendDMX bus
  
  return bus

closeBus :: Bus -> IO ()
closeBus bus = do
  hClose $ devH bus

sendDMX :: Bus -> IO ()
sendDMX bus = do
  hPutArray (devH bus) (states bus) dataLength

--setChannel :: (MArray IOUArray Word8 m) => Bus -> Int -> Word8 -> m ()
setChannel bus channel value = do
  writeArray (states bus) (channel+firstChan) value
