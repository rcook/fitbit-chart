{-# LANGUAGE DeriveGeneric #-}

module FitbitDemoLib.WeightSample
    ( WeightSample(..)
    ) where

import           Data.Aeson (ToJSON)
import           Data.Csv (ToRecord(..), record, toField)
import           Data.Time.Calendar (Day)
import           FitbitDemoLib.Format
import           GHC.Generics (Generic)

data WeightSample = WeightSample Day Double deriving Generic

instance ToRecord WeightSample where
    toRecord (WeightSample day value) = record [ toField (formatDay day), toField value ]

instance ToJSON WeightSample
