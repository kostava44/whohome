{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent.Async (concurrently_, race_)
import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, takeMVar)
import Control.Monad (forever)
import Control.Monad.Loops (whileJust_)
import Data.ByteString.Char8 qualified as B8
import Network.Simple.TCP qualified as TCP (HostName, Socket, connect, recv, send)
import Network.Telnet.LibTelnet qualified as Telnet

data State = State (MVar B8.ByteString) (MVar ())

telnetH :: State -> TCP.Socket -> Telnet.EventHandler
telnetH (State readBuf _) _ _ (Telnet.Received b) = putMVar readBuf b
telnetH _ s _ (Telnet.Send b) = TCP.send s b
telnetH _ _ _ _ = pure ()

handle :: State -> Telnet.Telnet -> TCP.Socket -> IO ()
handle (State _ done) telnet s = do
  whileJust_ (TCP.recv s 4096) $ \bs -> do
    Telnet.telnetRecv telnet bs
  putMVar done ()

connect :: TCP.HostName -> ((B8.ByteString -> IO (), IO B8.ByteString) -> IO a) -> IO ()
connect host f = do
  readBuf <- newEmptyMVar
  done <- newEmptyMVar
  let state = State readBuf done
  TCP.connect
    host
    "telnet"
    ( \(s, _) -> do
        telnet <- Telnet.telnetInit [] [] (telnetH state s)
        concurrently_
          (handle state telnet s)
          ( race_
              (takeMVar done)
              ( let send = Telnet.telnetSend telnet
                    recv = takeMVar readBuf
                 in f (send, recv)
              )
          )
    )

main :: IO ()
main = do
  connect "127.0.0.1" (\(_, recv) -> forever $ recv >>= B8.putStr)
