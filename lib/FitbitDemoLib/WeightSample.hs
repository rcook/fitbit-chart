module FitbitDemoLib.WeightSample
    ( WeightSample(..)
    ) where

import           Data.Csv (ToRecord(..), record, toField)
import           Data.Time.Calendar (Day)
import           FitbitDemoLib.Format

data WeightSample = WeightSample Day Double

instance ToRecord WeightSample where
    toRecord (WeightSample day value) = record [ toField (formatDay day), toField value ]
