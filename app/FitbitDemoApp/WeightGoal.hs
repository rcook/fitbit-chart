{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module FitbitDemoApp.WeightGoal
    ( getWeightGoal
    ) where

import           Data.Aeson ((.:), Value, withObject)
import           Data.Aeson.Types (Parser, parseEither)
import           FitbitDemoApp.Types
import           FitbitDemoApp.APIUtil
import           Network.HTTP.Req ((/:))

getWeightGoal :: APIAction WeightGoal
getWeightGoal apiUrl tokenConfig =
    parseEither pResponse
        <$> fitbitApiGet (apiUrl /: "user" /: "-" /: "body" /: "log" /: "weight" /: "goal.json") tokenConfig

pResponse :: Value -> Parser WeightGoal
pResponse = withObject "WeightGoalResponse" $ \v -> do
    obj <- v .: "goal"
    WeightGoal <$> obj .: "goalType" <*> obj .: "weight" <*> obj .: "startWeight"
