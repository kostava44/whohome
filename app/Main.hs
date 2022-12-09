{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent (threadDelay)
import Control.Monad (void)
import Data.ByteString.Char8 qualified as B8
import TelnetSimple qualified as Telnet (State, connect, recvAll, send)

sendCommand :: Telnet.State -> B8.ByteString -> IO [B8.ByteString]
sendCommand handle cmd = do
  threadDelay 100_000
  Telnet.send handle cmd
  threadDelay 100_000
  Telnet.recvAll handle

main :: IO ()
main =
  void $
    Telnet.connect
      "192.168.31.1"
      ( \telnet -> do
          sendCommand telnet "root\n" >>= (mapM_ B8.putStr)
      )
