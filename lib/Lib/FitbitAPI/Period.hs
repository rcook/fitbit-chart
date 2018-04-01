{-# LANGUAGE OverloadedStrings #-}

module Lib.FitbitAPI.Period
    ( Period(..)
    , formatPeriod
    ) where

import           Data.Text (Text)

data Period =
    OneDay
    | SevenDays
    | ThirtyDays
    | OneWeek
    | OneMonth
    | ThreeMonths
    | SixMonths
    | OneYear
    | Max

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
