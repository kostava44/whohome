{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (forever)
import Data.ByteString.Char8 qualified as B8
import TelnetSimple (connect)

main :: IO ()
main = do
  connect "127.0.0.1" (\(_, recv) -> forever $ recv >>= B8.putStr)
