module FitbitDemoLib.DateTime
    ( formatDay
    , mkDay
    ) where

import           Data.Text (Text)
import qualified Data.Text as Text (pack)
import           Data.Time.Calendar (Day, fromGregorian)

mkDay :: Int -> Int -> Int -> Day
mkDay year month date = fromGregorian (fromIntegral year) month date

formatDay :: Day -> Text
formatDay = Text.pack . show
