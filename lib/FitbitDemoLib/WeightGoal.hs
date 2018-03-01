{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module FitbitDemoLib.WeightGoal
    ( getWeightGoal
    , getWeightGoal'
    ) where

import           Data.Aeson ((.:), Value, withObject)
import           Data.Aeson.Types (Parser, parseEither)
import           FitbitDemoLib.OAuth2Helper
import           FitbitDemoLib.OAuth2Types
import           FitbitDemoLib.Types
import           Network.HTTP.Req
                    ( (/:)
                    , Scheme(..)
                    , Url
                    )
import qualified Network.HTTP.Req.OAuth2 as OAuth2
                    ( App(..)
                    , ClientPair(..)
                    , TokenPair(..)
                    )

getWeightGoal' :: Url 'Https -> UpdateTokenPair -> OAuth2.App -> OAuth2.ClientPair -> OAuth2.TokenPair -> IO (APIResult WeightGoal, OAuth2.TokenPair)
getWeightGoal' apiUrl = oAuth2GetWithRefresh pResponse (apiUrl /: "user" /: "-" /: "body" /: "log" /: "weight" /: "goal.json")

getWeightGoal :: APIAction WeightGoal
getWeightGoal apiUrl (OAuth2.TokenPair accessToken _) =
    parseEither pResponse
        <$> oAuth2Get (apiUrl /: "user" /: "-" /: "body" /: "log" /: "weight" /: "goal.json") accessToken

pResponse :: Value -> Parser WeightGoal
pResponse = withObject "WeightGoalResponse" $ \v -> do
    obj <- v .: "goal"
    WeightGoal <$> obj .: "goalType" <*> obj .: "weight" <*> obj .: "startWeight"
