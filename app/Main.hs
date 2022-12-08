{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent.Async ( concurrently_, race_ )
import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, takeMVar)
import Control.Monad (forever)
import Control.Monad.Loops (whileJust_)
import Data.ByteString.Char8 qualified as B8
import Network.Simple.TCP (Socket, connect, recv, send)
import Network.Telnet.LibTelnet qualified as Telnet

-- handle <- connect host
-- send handle bs
-- bs <- recv handle

telnetH :: MVar B8.ByteString -> Socket -> Telnet.EventHandler
telnetH state _ _ (Telnet.Received b) =
  putStr "R: " *> B8.putStrLn b *> putMVar state b
telnetH _ s _ (Telnet.Send b) =
  putStr "S: " *> send s b
telnetH _ _ _ _ = pure ()

handle :: MVar B8.ByteString -> MVar () -> Socket -> IO ()
handle state done s = do
  telnet <- Telnet.telnetInit [] [] (telnetH state s)
  whileJust_ (recv s 4096) $ \bs -> do
    Telnet.telnetRecv telnet bs
  putMVar done ()

main :: IO ()
main = do
  state <- newEmptyMVar
  done <- newEmptyMVar
  concurrently_
    (connect "127.0.0.1" "telnet" (\(s, _) -> handle state done s))
    ( race_
        (takeMVar done)
        (forever $ takeMVar state >>= B8.putStrLn)
    )
