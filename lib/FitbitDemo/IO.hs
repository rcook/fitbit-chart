module FitbitDemo.IO
    ( decodeJSON
    , decodeYAML
    , encodeJSON
    , encodeYAML
    ) where

import           Data.Aeson (FromJSON, ToJSON)
import qualified Data.Aeson as Aeson (decodeStrict, encode)
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as ByteString (toStrict)
import qualified Data.Yaml as Yaml (decode, encode)

encodeJSON :: ToJSON a => a -> ByteString
encodeJSON = ByteString.toStrict . Aeson.encode

decodeJSON :: FromJSON a => ByteString -> Maybe a
decodeJSON = Aeson.decodeStrict

encodeYAML :: ToJSON a => a -> ByteString
encodeYAML = Yaml.encode

decodeYAML :: FromJSON a => ByteString -> Maybe a
decodeYAML = Yaml.decode