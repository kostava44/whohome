{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Config
import Control.Concurrent (threadDelay)
import Control.Monad (void)
import Data.Aeson qualified as Aeson
import Data.ByteString.Char8 qualified as B8
import Data.ByteString.Lazy qualified as BL
import Data.HashMap.Strict qualified as HashMap
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import ParseMACs qualified as Parse
import TelnetSimple qualified as Telnet (State, connect, recvAll, send)
import Text.Megaparsec hiding (State, label)

sendCommand :: Telnet.State -> B8.ByteString -> IO B8.ByteString
sendCommand handle cmd = do
  threadDelay 100_000
  Telnet.send handle (cmd <> "\n")
  threadDelay 100_000
  Telnet.recvAll handle

voidCmd :: Telnet.State -> B8.ByteString -> IO ()
voidCmd = (fmap . fmap) void sendCommand

fetchMacs :: Host -> IO [B8.ByteString]
fetchMacs Host {..} =
  Telnet.connect
    hostname
    ( \telnet -> do
        voidCmd telnet username
        voidCmd telnet password

        buf <- sendCommand telnet "iwinfo wl0 assoclist"
        pure $
          Parse.encode <$> case parse Parse.macsLenient "" buf of
            Left e -> fail (errorBundlePretty e)
            Right x -> x
    )

processHost :: Config -> Host -> IO (Text, [Text])
processHost Config {mac_names} host = do
  macs <- fetchMacs host
  let macsWithNames = [HashMap.findWithDefault (T.decodeUtf8 m) m mac_names | m <- macs]
  let l = Config.label host
  pure (l, macsWithNames)

main :: IO ()
main = do
  config@Config {..} <-
    (B8.getContents >>=) $
      (. Aeson.eitherDecodeStrict) $ \case
        Left e -> fail e
        Right x -> pure x
  mapM (processHost config) hosts >>= BL.putStr . Aeson.encode . HashMap.fromList
