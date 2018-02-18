module FitbitDemo.IO
    ( decodeJSON
    , decodeYAML
    , decodeYAMLFile
    , encodeJSON
    , encodeYAML
    , encodeYAMLFile
    ) where

import           Data.Aeson (FromJSON, ToJSON)
import qualified Data.Aeson as Aeson (decodeStrict, encode)
import           Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString (writeFile)
import qualified Data.ByteString.Lazy as ByteString (toStrict)
import qualified Data.Yaml as Yaml (decode, decodeFile, encode, encodeFile)

encodeJSON :: ToJSON a => a -> ByteString
encodeJSON = ByteString.toStrict . Aeson.encode

decodeJSON :: FromJSON a => ByteString -> Maybe a
decodeJSON = Aeson.decodeStrict

encodeYAML :: ToJSON a => a -> ByteString
encodeYAML = Yaml.encode

encodeYAMLFile :: ToJSON a => FilePath -> a -> IO ()
encodeYAMLFile = Yaml.encodeFile

decodeYAML :: FromJSON a => ByteString -> Maybe a
decodeYAML = Yaml.decode

decodeYAMLFile :: FromJSON a => FilePath -> IO (Maybe a)
decodeYAMLFile = Yaml.decodeFile