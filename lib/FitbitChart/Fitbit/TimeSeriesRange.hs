module FitbitChart.Fitbit.TimeSeriesRange
    ( TimeSeriesRange(..)
    ) where

import           Data.Time.Calendar (Day)
import           FitbitChart.Fitbit.Period

data TimeSeriesRange =
    Ending Day Period
    | Between Day Day
