module Main where

import Network.Socket
import System.Posix.Directory
import System.Posix.Files
import System.Posix.IO
import System.Posix.Process
import System.Exit
import Data.Time ()
import Data.Time.Clock (getCurrentTime) -- For error msgs.
import Control.Monad (when)
import System.IO (hPutStrLn, stderr)
import System.Environment (getArgs)

import DMX
import LightSource
import Configuration

-- TODO Until something better is done, this may serve the purpose of
-- main function.
main = do
  args <- getArgs
  let dev = case args of
              [x] -> x
              [] -> "/dev/serial/by-id/usb-ENTTEC_DMX_USB_PRO_ENR35XBU-if00-port0"
              _ -> error "Usage: UDPCommander [device]"

  lightServer dev

--toLightArray :: Bus -> [(Word8,Word8)] -> [(Char, Light)]
toLightArray bus original = map fix original
  where fix (logical,dmx) = (enumMangle logical, Light (enumMangle dmx) bus)

lightServer :: FilePath -> IO ()
lightServer dev = do
  bus <- createBus dev initialValues
  let lightArray = toLightArray bus lights
  withSocketsDo $ do
                sock <- socket AF_INET Datagram 0
                bindSocket sock (SockAddrInet udpPort iNADDR_ANY)
                socketHandler (lightParser lightArray) sock

-- |Handles incoming UDP packets.
socketHandler :: (String -> IO ()) -> Socket -> IO ()
socketHandler parseF sock = do
  (mesg, recv_count, client) <- recvFrom sock maxline
  parseF mesg
  socketHandler parseF sock

-- |Parses single packet and performs the actions needed.  FIXME: This
-- is only a hack to support multiple commands and new packet
-- format. Rewrite is coming.
lightParser :: [(Char, Light)] -> [Char] -> IO ()
lightParser lights ('\x00':'\x00':'\x00':'\x00':'\x00':'\x00':'\x00':lightB:r:g:b:next) = do
  case lookup lightB lights of
    Nothing -> logError $ "Unknown lamp: " ++ show (fromEnum lightB)
    Just light -> setLight light (Color (enumMangle r)
                                        (enumMangle g)
                                        (enumMangle b))
  lightParser lights next
lightParser lights [] = sendDMX (getBusStupidWay lights) -- End of multi-command. Flush lights.
lightParser _ x = logError $ "Incoming command is broken. Length: " ++ show (length x)

getBusStupidWay ((_, (Light _ bus)):xs) = bus
  
-- |Just writes errors to the console.
logError :: [Char] -> IO ()
logError msg = do
  time <- getCurrentTime
  hPutStrLn stderr $ show time ++ " " ++ msg

-- |Argh. Stupid Haskell and it's antiquated socket functions...
enumMangle :: (Enum a, Enum b) => a -> b
enumMangle = toEnum . fromEnum
