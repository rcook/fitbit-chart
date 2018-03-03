{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module FitbitDemoLib.WeightGoal
    ( getWeightGoal
    ) where

import           Data.Aeson ((.:), Value, withObject)
import           Data.Aeson.Types (Parser)
import           FitbitDemoLib.Types
import           Network.HTTP.Req ((/:))
import           Network.HTTP.Req.OAuth2 (APIAction, oAuth2Get)

getWeightGoal :: APIAction WeightGoal
getWeightGoal apiUrl =
    oAuth2Get pResponse (apiUrl /: "user" /: "-" /: "body" /: "log" /: "weight" /: "goal.json")

pResponse :: Value -> Parser WeightGoal
pResponse = withObject "WeightGoalResponse" $ \v -> do
    obj <- v .: "goal"
    WeightGoal <$> obj .: "goalType" <*> obj .: "weight" <*> obj .: "startWeight"
