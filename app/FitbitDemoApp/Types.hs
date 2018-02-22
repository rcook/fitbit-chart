module FitbitDemoApp.Types
    ( APIAction
    , Period(..)
    , TimeSeriesRange(..)
    , WeightSample(..)
    ) where

import           Data.Time.Calendar (Day)
import           FitbitDemoLib

data Period = OneDay | SevenDays | ThirtyDays | OneWeek | OneMonth | ThreeMonths | SixMonths | OneYear | Max

data TimeSeriesRange = Ending Day Period | Between Day Day

data WeightSample = WeightSample Day Double

type APIAction a = TokenConfig -> IO a
