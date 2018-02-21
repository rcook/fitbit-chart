{-# LANGUAGE OverloadedStrings #-}

module FitbitDemoLib.DateTime
    ( formatDay
    , mkDay
    , parseDay
    ) where

import           Data.Text (Text)
import qualified Data.Text as Text (pack, splitOn)
import qualified Data.Text.Read as Text (decimal)
import           Data.Time.Calendar (Day, fromGregorian)

mkDay :: Int -> Int -> Int -> Day
mkDay year month date = fromGregorian (fromIntegral year) month date

parseDay :: Text -> Maybe Day
parseDay s =
    case Text.splitOn "-" s of
        (yyyy : mm : dd : []) -> do
            let Right (y, "") = Text.decimal yyyy -- TODO:
                Right (m, "") = Text.decimal mm -- TODO:
                Right (d, "") = Text.decimal dd -- TODO:
            return $ mkDay y m d
        _ -> Nothing

formatDay :: Day -> Text
formatDay = Text.pack . show
