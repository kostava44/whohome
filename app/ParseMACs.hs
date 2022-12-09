{-# LANGUAGE OverloadedStrings #-}

module ParseMACs where

import Control.Monad (void)
import Data.ByteString.Char8 qualified as B8
import Data.ByteString.Internal (c2w)
import Data.Void (Void)
import Data.Word (Word8)
import Net.Mac qualified as Mac
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Byte

main :: IO ()
main = do
  let Just mac = Mac.decodeUtf8 "de:ad:be:ef:ff:ff"
  Mac.print mac
