{-# LANGUAGE OverloadedStrings #-}

module FitbitDemoApp.Util
    ( formatPeriod
    ) where

import           Data.Text (Text)
import           FitbitDemoApp.Types

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
