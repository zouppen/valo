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

import DMX
import LightSource

udpPort = 9909
maxline = 1500

lightServer :: IO ()
lightServer = do
  bus <- createBus "/dev/serial/by-id/usb-ENTTEC_DMX_USB_PRO_ENR35XBU-if00-port0"
  let lights = [('\x00',Light 1 bus)
               ,('\x01',Light 6 bus)
               ]

  withSocketsDo $ do
                sock <- socket AF_INET Datagram 0
                bindSocket sock (SockAddrInet udpPort iNADDR_ANY)
                socketHandler (lightParser lights) sock

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
    Just light -> setLightInstantly light (Color (enumMangle r)
                                                 (enumMangle g)
                                                 (enumMangle b))
  lightParser lights next
lightParser _ [] = return () -- End of multi-command
lightParser _ x = logError $ "Incoming command is broken. Length: " ++ show (length x)
  
-- |Just writes errors to the console.
logError :: [Char] -> IO ()
logError msg = do
  time <- getCurrentTime
  hPutStrLn stderr $ show time ++ " " ++ msg

-- |Argh. Stupid Haskell and it's antiquated socket functions...
enumMangle = toEnum . fromEnum
