{-# OPTIONS_GHC -Wno-orphans #-}

module Config where

import Data.Aeson (FromJSON (..), FromJSONKey (..), FromJSONKeyFunction (..), ToJSON (..))
import Data.ByteString.Char8 qualified as B8
import Data.HashMap.Strict (HashMap)
import Data.Text (Text)
import Data.Text.Encoding qualified as T
import GHC.Generics
import Network.Simple.TCP (HostName)

instance ToJSON B8.ByteString where
  toJSON = toJSON . T.decodeUtf8

instance FromJSON B8.ByteString where
  parseJSON o = T.encodeUtf8 <$> parseJSON o

instance FromJSONKey B8.ByteString where
  fromJSONKey = FromJSONKeyText T.encodeUtf8

data Config = Config
  { hosts :: [Host],
    mac_names :: HashMap B8.ByteString Text
  }
  deriving (Generic, Show)

instance FromJSON Config

data Host = Host
  { label :: Text,
    hostname :: HostName,
    username :: B8.ByteString,
    password :: B8.ByteString
  }
  deriving (Generic, Show)

instance ToJSON Host

instance FromJSON Host
