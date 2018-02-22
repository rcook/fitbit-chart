{-# LANGUAGE OverloadedStrings #-}

module FitbitDemoLib.DateTime
    ( formatDay
    , mkDay
    , parseDay
    ) where

import           Data.Text (Text)
import qualified Data.Text as Text (pack, splitOn, unpack)
import           Data.Time.Calendar (Day, fromGregorian)
import           FitbitDemoLib.Util

mkDay :: Int -> Int -> Int -> Day
mkDay year month date = fromGregorian (fromIntegral year) month date

parseDay :: Text -> Either String Day
parseDay s =
    case Text.splitOn "-" s of
        (yyyy : mm : dd : []) -> mkDay <$> parseInt yyyy <*> parseInt mm <*> parseInt dd
        _ -> Left $ "Could not parse day from " ++ Text.unpack s

formatDay :: Day -> Text
formatDay = Text.pack . show
