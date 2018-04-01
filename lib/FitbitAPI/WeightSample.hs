{-# LANGUAGE DeriveGeneric #-}

module FitbitAPI.WeightSample
    ( WeightSample(..)
    ) where

import           Data.Aeson (ToJSON)
import           Data.Csv (ToRecord(..), record, toField)
import           Data.Time.Calendar (Day)
import           Util.Format (formatDay)
import           GHC.Generics (Generic)

data WeightSample = WeightSample Day Double deriving Generic

instance ToRecord WeightSample where
    toRecord (WeightSample day value) = record [ toField (formatDay day), toField value ]

instance ToJSON WeightSample
