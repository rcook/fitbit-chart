{-# LANGUAGE OverloadedStrings #-}

module FitbitAPI.Format
    ( formatDay
    , formatPeriod
    ) where

import           Data.Text (Text)
import qualified Data.Text as Text (pack)
import           Data.Time.Calendar (Day)
import           FitbitAPI.Types

formatPeriod :: Period -> Text
formatPeriod OneDay = "1d"
formatPeriod SevenDays = "7d"
formatPeriod ThirtyDays = "30d"
formatPeriod OneWeek = "1w"
formatPeriod OneMonth = "1m"
formatPeriod ThreeMonths = "3m"
formatPeriod SixMonths = "6m"
formatPeriod OneYear = "1y"
formatPeriod Max = "max"

formatDay :: Day -> Text
formatDay = Text.pack . show
