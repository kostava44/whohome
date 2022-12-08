{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent.Async (concurrently_, race_)
import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, takeMVar)
import Control.Monad (forever)
import Control.Monad.Loops (whileJust_)
import Data.ByteString.Char8 qualified as B8
import Network.Simple.TCP qualified as TCP (Socket, connect, recv, send)
import Network.Telnet.LibTelnet qualified as Telnet

-- handle <- connect host
-- send handle bs
-- bs <- recv handle

data State = State (MVar B8.ByteString) (MVar ())

telnetH :: State -> TCP.Socket -> Telnet.EventHandler
telnetH (State readBuf _) _ _ (Telnet.Received b) = putMVar readBuf b
telnetH _ s _ (Telnet.Send b) =
  putStr "S: " *> TCP.send s b
telnetH _ _ _ _ = pure ()

handle :: State -> TCP.Socket -> IO ()
handle state@(State _ done) s = do
  telnet <- Telnet.telnetInit [] [] (telnetH state s)
  whileJust_ (TCP.recv s 4096) $ \bs -> do
    Telnet.telnetRecv telnet bs
  putMVar done ()

recv :: State -> IO B8.ByteString
recv (State readBuf _) = takeMVar readBuf

main :: IO ()
main = do
  x <- newEmptyMVar
  done <- newEmptyMVar
  let state = State x done
  concurrently_
    (TCP.connect "127.0.0.1" "telnet" (\(s, _) -> handle state s))
    ( race_
        (takeMVar done)
        (forever $ recv state >>= B8.putStr)
    )
