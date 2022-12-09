{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent (threadDelay)
import Control.Monad (void)
import Data.ByteString.Char8 qualified as B8
import Secret qualified
import TelnetSimple qualified as Telnet (State, connect, recvAll, send)

sendCommand :: Telnet.State -> B8.ByteString -> IO B8.ByteString
sendCommand handle cmd = do
  threadDelay 100_000
  Telnet.send handle cmd
  threadDelay 100_000
  Telnet.recvAll handle

voidCmd :: Telnet.State -> B8.ByteString -> IO ()
voidCmd = (fmap . fmap) void sendCommand

main :: IO ()
main =
  void $
    Telnet.connect
      Secret.destHost
      ( \telnet -> do
          Secret.auth (voidCmd telnet)
          buf <- sendCommand telnet "iwinfo wl0 assoclist\n"
          B8.putStr buf
          voidCmd telnet "exit\n"
      )
