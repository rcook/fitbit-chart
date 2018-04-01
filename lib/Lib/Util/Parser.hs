{-# LANGUAGE OverloadedStrings #-}

module Lib.Util.Parser
    ( parseDay
    , parseDouble
    , parseInt
    ) where

import           Data.Text (Text)
import qualified Data.Text as Text (splitOn, unpack)
import qualified Data.Text.Read as Text (decimal, double)
import           Data.Time.Calendar (Day, fromGregorian)

parseInt :: Text -> Either String Int
parseInt s =
    case Text.decimal s of
        Right (result, "") -> return result
        _ -> Left $ "Could not parse int from " ++ Text.unpack s

parseDouble :: Text -> Either String Double
parseDouble s =
    case Text.double s of
        Right (result, "") -> return result
        _ -> Left $ "Could not parse double from " ++ Text.unpack s

parseDay :: Text -> Either String Day
parseDay s =
    case Text.splitOn "-" s of
        (yyyy : mm : dd : []) -> fromGregorian <$> (fromIntegral <$> parseInt yyyy) <*> parseInt mm <*> parseInt dd
        _ -> Left $ "Could not parse day from " ++ Text.unpack s
