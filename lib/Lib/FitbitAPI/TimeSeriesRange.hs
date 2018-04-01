module Lib.FitbitAPI.TimeSeriesRange
    ( TimeSeriesRange(..)
    ) where

import           Data.Time.Calendar (Day)
import           Lib.FitbitAPI.Period

data TimeSeriesRange =
    Ending Day Period
    | Between Day Day
