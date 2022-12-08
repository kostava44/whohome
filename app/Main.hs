{-# LANGUAGE OverloadedStrings#-}

module Main where

import qualified Data.ByteString.Char8 as B8
import           Network.Simple.TCP (Socket, send, connect)
import qualified Network.Telnet.LibTelnet as Telnet


telnetH :: Socket -> Telnet.EventHandler
telnetH _ t (Telnet.Received b)
  = putStr "R: " *> B8.putStrLn b *> Telnet.telnetSend t b
telnetH s _ (Telnet.Send b)
  = putStr "S: " *> send s b
telnetH _ _ _ = pure ()

main :: IO ()
main = connect "127.0.0.1" "telnet" (\(s, _) -> handle s) where
  handle s = do
      telnet <- Telnet.telnetInit [] [] (telnetH s)
      Telnet.telnetSend telnet "lalala"