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
import Data.Word

import DMX
import LightSource

udpPort = 9909
maxline = 1500

-- TODO Until something better is done, this may serve the purpose of
-- main function.
main = do
  args <- getArgs
  let dev = case args of
              [x] -> x
              [] -> "/dev/serial/by-id/usb-ENTTEC_DMX_USB_PRO_ENR35XBU-if00-port0"
              _ -> error "Usage: UDPCommander [device]"

  lightServer dev

-- |Lights in format of (logical channel, DMX start channel). FIXME stupid types.
lights :: (Integral a, Integral b) => [(a,b)]
lights = [(0,1)
         ,(1,26)
         ,(2,11)
         ,(3,6)
         ,(4,42)
         ,(5,31)
         ,(6,16)
         ,(7,36)
         ,(8,64)
         ,(9,(64+8))
         ,(10,(64+16))
         ,(11,(64+32))
         ,(12,(64+36))
         ]

-- |Values are initially zeros. 
--initialValues :: (Integral a) => [(a,Word8)]
initialValues = [(31,34) -- wash 1 positions
                ,(32,50)
                ,(33,77)
                ,(34,50)
                ,(49,255) -- wash 1 dimmer
                ,(51,34) -- wash 2 positions
                ,(52,50)
                ,(53,77)
                ,(54,50)
                ,(69,255) -- wash 2 dimmer
                ,(71,34) -- wash 3 positions
                ,(72,50)
                ,(73,77)
                ,(74,50)
                ,(89,255) -- wash 3 dimmer
                ,(91,34) -- wash 4 positions
                ,(92,50)
                ,(93,77)
                ,(94,50)
                ,(109,34) --wash 4 positions
                ]

--initialValues = []

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
