module FitbitAPI.TimeSeriesRange
    ( TimeSeriesRange(..)
    ) where

import           Data.Time.Calendar (Day)
import           FitbitAPI.Period

data TimeSeriesRange =
    Ending Day Period
    | Between Day Day
