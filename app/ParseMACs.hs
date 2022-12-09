{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}

module ParseMACs where

import Control.Monad (void)
import Data.Bits
import Data.ByteString.Char8 qualified as B8
import Data.ByteString.Internal (c2w)
import Data.Void (Void)
import Data.Word (Word64, Word8)
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Byte

-- https://hackage.haskell.org/package/ip-1.7.6
newtype Mac = Mac Word64 deriving (Show)

-- Unchecked invariant: each of these Word64s must be smaller
-- than 256.
unsafeWord48FromOctets :: Word64 -> Word64 -> Word64 -> Word64 -> Word64 -> Word64 -> Word64
unsafeWord48FromOctets a b c d e f =
  fromIntegral $
    unsafeShiftL a 40
      .|. unsafeShiftL b 32
      .|. unsafeShiftL c 24
      .|. unsafeShiftL d 16
      .|. unsafeShiftL e 8
      .|. f

-- | Create a 'Mac' address from six octets.
fromOctets :: Word8 -> Word8 -> Word8 -> Word8 -> Word8 -> Word8 -> Mac
fromOctets a b c d e f =
  Mac $
    unsafeWord48FromOctets
      (fromIntegral a)
      (fromIntegral b)
      (fromIntegral c)
      (fromIntegral d)
      (fromIntegral e)
      (fromIntegral f)

twoHex :: Parsec Void B8.ByteString Word8
twoHex = do
  a <- hexDigitChar
  b <- hexDigitChar
  pure (unsafeShiftL a 4 + b)

p :: Parsec Void B8.ByteString Mac
p =
  fromOctets
    <$> twoHex <* char (c2w ':')
    <*> twoHex <* char (c2w ':')
    <*> twoHex <* char (c2w ':')
    <*> twoHex <* char (c2w ':')
    <*> twoHex <* char (c2w ':')
    <*> twoHex

main :: IO ()
main = parseTest (p <* eof) "de:ad:be:ef:ff:ff"