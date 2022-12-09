{-# LANGUAGE OverloadedStrings #-}

module ParseMACs (Mac, mac, macsLenient, encode) where

import Control.Monad (void)
import Data.Bits (Bits (unsafeShiftL, unsafeShiftR, (.|.)))
import Data.ByteString.Builder qualified as BB
import Data.ByteString.Char8 qualified as B8
import Data.ByteString.Internal (c2w)
import Data.ByteString.Lazy qualified as BL
import Data.Void (Void)
import Data.Word (Word64, Word8)
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Byte

-- https://hackage.haskell.org/package/megaparsec-9.3.0/docs/src/Text.Megaparsec.Byte.Lexer.html#hexadecimal
w2h :: Word8 -> Word8
w2h w
  | w >= 48 && w <= 57 = w - 48
  | w >= 65 && w <= 70 = w - 55
  | w >= 97 && w <= 102 = w - 87
  | otherwise = w

-- https://hackage.haskell.org/package/ip-1.7.6
newtype Mac = Mac Word64 deriving (Show, Eq)

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

-- prop> m == (let (a,b,c,d,e,f) = toOctets m in fromOctets a b c d e f)
toOctets :: Mac -> (Word8, Word8, Word8, Word8, Word8, Word8)
toOctets (Mac w) =
  ( fromIntegral $ unsafeShiftR w 40,
    fromIntegral $ unsafeShiftR w 32,
    fromIntegral $ unsafeShiftR w 24,
    fromIntegral $ unsafeShiftR w 16,
    fromIntegral $ unsafeShiftR w 8,
    fromIntegral w
  )

encode :: Mac -> B8.ByteString
encode m =
  BL.toStrict $
    BB.toLazyByteString $
      let (a, b, c, d, e, f) = toOctets m
       in BB.word8Hex a
            <> BB.char8 ':'
            <> BB.word8Hex b
            <> BB.char8 ':'
            <> BB.word8Hex c
            <> BB.char8 ':'
            <> BB.word8Hex d
            <> BB.char8 ':'
            <> BB.word8Hex e
            <> BB.char8 ':'
            <> BB.word8Hex f

twoHex :: Parsec Void B8.ByteString Word8
twoHex = do
  a <- w2h <$> hexDigitChar
  b <- w2h <$> hexDigitChar
  pure (unsafeShiftL a 4 + b)

mac :: Parsec Void B8.ByteString Mac
mac =
  fromOctets
    <$> twoHex <* char (c2w ':')
    <*> twoHex <* char (c2w ':')
    <*> twoHex <* char (c2w ':')
    <*> twoHex <* char (c2w ':')
    <*> twoHex <* char (c2w ':')
    <*> twoHex

macLenient :: Parsec Void B8.ByteString Mac
macLenient = void (manyTill anySingle (lookAhead . try $ mac)) *> mac

macsLenient :: Parsec Void B8.ByteString [Mac]
macsLenient = many . try $ macLenient
