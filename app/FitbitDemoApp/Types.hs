{-# LANGUAGE DataKinds #-}

module FitbitDemoApp.Types
    ( APIAction
    , Period(..)
    , TimeSeriesRange(..)
    , WeightGoal(..)
    , WeightSample(..)
    ) where

import           Data.Text (Text)
import           Data.Time.Calendar (Day)
import           FitbitDemoLib
import           Network.HTTP.Req
                    ( Scheme(..)
                    , Url
                    )

data Period = OneDay | SevenDays | ThirtyDays | OneWeek | OneMonth | ThreeMonths | SixMonths | OneYear | Max

data TimeSeriesRange = Ending Day Period | Between Day Day

data WeightGoal = WeightGoal
    { goalType :: Text
    , goalWeight :: Double
    , startWeight :: Double
    }

data WeightSample = WeightSample Day Double

type APIResult a = Either String a

type APIAction a = Url 'Https -> TokenConfig -> IO (APIResult a)
