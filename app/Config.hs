{-# OPTIONS_GHC -Wno-orphans #-}

module Config where

import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.ByteString.Char8 qualified as B8
import Data.Text (Text)
import Data.Text.Encoding qualified as T
import GHC.Generics
import Network.Simple.TCP (HostName)

instance ToJSON B8.ByteString where
  toJSON = toJSON . T.decodeUtf8

instance FromJSON B8.ByteString where
  parseJSON o = T.encodeUtf8 <$> parseJSON o

data Host = Host
  { label :: Text,
    hostname :: HostName,
    username :: B8.ByteString,
    password :: B8.ByteString
  }
  deriving (Generic, Show)

instance ToJSON Host

instance FromJSON Host
