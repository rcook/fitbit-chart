module FitbitDemoLib.Types
    ( Period(..)
    , TimeSeriesRange(..)
    , WeightGoal(..)
    , WeightSample(..)
    ) where

import           Data.Csv (ToRecord(..), toField, record)
import           Data.Text (Text)
import           Data.Time.Calendar (Day)
import           FitbitDemoLib.DateTime (formatDay)
import           GHC.Generics (Generic)

data Period = OneDay | SevenDays | ThirtyDays | OneWeek | OneMonth | ThreeMonths | SixMonths | OneYear | Max

data TimeSeriesRange = Ending Day Period | Between Day Day

data WeightGoal = WeightGoal
    { goalType :: Text
    , goalWeight :: Double
    , startWeight :: Double
    }

data WeightSample = WeightSample Day Double

instance ToRecord WeightSample where
    toRecord (WeightSample day value) = record [ toField (formatDay day), toField value ]
