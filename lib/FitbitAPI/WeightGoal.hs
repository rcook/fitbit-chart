{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module FitbitAPI.WeightGoal
    ( getWeightGoal
    ) where

import           Data.Aeson ((.:), Value, withObject)
import           Data.Aeson.Types (Parser)
import           FitbitAPI.Types
import           Network.HTTP.Req ((/:), Scheme(..), Url)
import           Network.HTTP.Req.OAuth2 (App, OAuth2, oAuth2Get)

getWeightGoal :: App -> Url 'Https -> OAuth2 WeightGoal
getWeightGoal app apiUrl =
    oAuth2Get pResponse (apiUrl /: "user" /: "-" /: "body" /: "log" /: "weight" /: "goal.json") app

pResponse :: Value -> Parser WeightGoal
pResponse = withObject "WeightGoalResponse" $ \v -> do
    obj <- v .: "goal"
    WeightGoal <$> obj .: "goalType" <*> obj .: "weight" <*> obj .: "startWeight"
