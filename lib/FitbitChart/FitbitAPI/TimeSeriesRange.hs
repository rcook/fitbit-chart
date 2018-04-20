module FitbitChart.FitbitAPI.TimeSeriesRange
    ( TimeSeriesRange(..)
    ) where

import           Data.Time.Calendar (Day)
import           FitbitChart.FitbitAPI.Period

data TimeSeriesRange =
    Ending Day Period
    | Between Day Day
