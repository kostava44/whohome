{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (concurrently_)
import Control.Monad (forever, void)
import Data.ByteString.Char8 qualified as B8
import TelnetSimple (connect)

main :: IO ()
main =
  void $
    connect
      "127.0.0.1"
      ( \(send, recv) ->
          concurrently_
            (forever $ recv >>= B8.putStr)
            ( do
                threadDelay 100_000
                send "root\n"
            )
      )
