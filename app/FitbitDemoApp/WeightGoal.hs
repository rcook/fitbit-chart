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
import qualified Network.HTTP.Req.OAuth2 as OAuth2 (TokenPair(..))

getWeightGoal :: APIAction WeightGoal
getWeightGoal apiUrl (OAuth2.TokenPair accessToken _) =
    parseEither pResponse
        <$> fitbitApiGet (apiUrl /: "user" /: "-" /: "body" /: "log" /: "weight" /: "goal.json") accessToken

pResponse :: Value -> Parser WeightGoal
pResponse = withObject "WeightGoalResponse" $ \v -> do
    obj <- v .: "goal"
    WeightGoal <$> obj .: "goalType" <*> obj .: "weight" <*> obj .: "startWeight"
