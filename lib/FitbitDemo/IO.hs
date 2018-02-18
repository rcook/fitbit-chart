module FitbitDemo.IO
    ( encodeJson
    , encodeYaml
    ) where

import           Data.Aeson (ToJSON)
import qualified Data.Aeson as Aeson (encode)
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as ByteString (toStrict)
import qualified Data.Yaml as Yaml (encode)

encodeJson :: ToJSON a => a -> ByteString
encodeJson = ByteString.toStrict . Aeson.encode

encodeYaml :: ToJSON a => a -> ByteString
encodeYaml = Yaml.encode
