module Main where

import Network.Socket
import System.Posix.Directory
import System.Posix.Files
import System.Posix.IO
import System.Posix.Process
import System.Exit


echoPort = 9909
maxline = 1500

--
-- The echo server daemon
--

echoserver :: IO ()
echoserver = do
           withSocketsDo $ do
                   sock <- socket AF_INET Datagram 0
                   bindSocket sock (SockAddrInet echoPort iNADDR_ANY)
                   socketEcho sock


socketEcho :: Socket -> IO ()
socketEcho sock = do
           (mesg, recv_count, client) <- recvFrom sock maxline
           putStrLn mesg
           putStrLn $ show recv_count
           putStrLn $ show client
           socketEcho sock

