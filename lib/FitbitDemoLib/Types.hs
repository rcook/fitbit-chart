{-# LANGUAGE DataKinds #-}

module FitbitDemoLib.Types
    ( APIAction
    , Period(..)
    , TimeSeriesRange(..)
    , WeightGoal(..)
    , WeightSample(..)
    ) where

import           Data.Text (Text)
import           Data.Time.Calendar (Day)
import           Network.HTTP.Req
                    ( Scheme(..)
                    , Url
                    )
import qualified Network.HTTP.Req.OAuth2 as OAuth2 (TokenPair)

data Period = OneDay | SevenDays | ThirtyDays | OneWeek | OneMonth | ThreeMonths | SixMonths | OneYear | Max

data TimeSeriesRange = Ending Day Period | Between Day Day

data WeightGoal = WeightGoal
    { goalType :: Text
    , goalWeight :: Double
    , startWeight :: Double
    }

data WeightSample = WeightSample Day Double

type APIResult a = Either String a

type APIAction a = Url 'Https -> OAuth2.TokenPair -> IO (APIResult a)
