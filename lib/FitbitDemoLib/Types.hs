module FitbitDemoLib.Types
    ( Period(..)
    , TimeSeriesRange(..)
    , WeightGoal(..)
    , WeightSample(..)
    ) where

import           Data.Text (Text)
import           Data.Time.Calendar (Day)

data Period = OneDay | SevenDays | ThirtyDays | OneWeek | OneMonth | ThreeMonths | SixMonths | OneYear | Max

data TimeSeriesRange = Ending Day Period | Between Day Day

data WeightGoal = WeightGoal
    { goalType :: Text
    , goalWeight :: Double
    , startWeight :: Double
    }

data WeightSample = WeightSample Day Double
