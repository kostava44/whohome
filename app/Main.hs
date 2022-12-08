{-# LANGUAGE OverloadedStrings#-}

module Main where

import Control.Monad.Loops (whileJust_)
import qualified Data.ByteString.Char8 as B8
import           Network.Simple.TCP (Socket, recv, send, connect)
import qualified Network.Telnet.LibTelnet as Telnet


telnetH :: Socket -> Telnet.EventHandler
telnetH _ _ (Telnet.Received b)
  = putStr "R: " *> B8.putStrLn b
telnetH s _ (Telnet.Send b)
  = putStr "S: " *> send s b
telnetH _ _ _ = pure ()

main :: IO ()
main = connect "127.0.0.1" "telnet" (\(s, _) -> handle s) where
  handle s = do
      telnet <- Telnet.telnetInit [] [] (telnetH s)
      whileJust_ (recv s 4096) $ \bs -> do
          Telnet.telnetRecv telnet bs
