{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module FitbitChart.Fitbit.WeightGoal
    ( WeightGoal
    , getWeightGoal
    , goalType
    , goalWeight
    , startWeight
    ) where

import           Data.Aeson ((.:), Value, withObject)
import           Data.Aeson.Types (Parser)
import           Data.Text (Text)
import           Network.HTTP.Req ((/:), Scheme(..), Url)
import           Network.HTTP.Req.OAuth2 (App, OAuth2, oAuth2Get)

data WeightGoal = WeightGoal
    { goalType :: Text
    , goalWeight :: Double
    , startWeight :: Double
    }

getWeightGoal :: App -> Url 'Https -> OAuth2 WeightGoal
getWeightGoal app apiUrl =
    oAuth2Get pResponse (apiUrl /: "user" /: "-" /: "body" /: "log" /: "weight" /: "goal.json") app

pResponse :: Value -> Parser WeightGoal
pResponse = withObject "WeightGoalResponse" $ \v -> do
    obj <- v .: "goal"
    WeightGoal <$> obj .: "goalType" <*> obj .: "weight" <*> obj .: "startWeight"
