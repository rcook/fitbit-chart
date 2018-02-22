{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module FitbitDemoApp.WeightGoal
    ( getWeightGoal
    ) where

import           Data.Aeson ((.:), Value, withObject)
import           Data.Aeson.Types (Parser, parseEither)
import           Data.Default.Class (def)
import           Data.Monoid ((<>))
import           FitbitDemoApp.Types
import           FitbitDemoLib
import           Network.HTTP.Req
                    ( (/:)
                    , GET(..)
                    , NoReqBody(..)
                    , jsonResponse
                    , req
                    , responseBody
                    , runReq
                    )

getWeightGoal :: APIAction WeightGoal
getWeightGoal apiUrl tokenConfig = do
    body <- responseBody <$> (runReq def $
                req GET
                    (apiUrl /: "user" /: "-" /: "body" /: "log" /: "weight" /: "goal.json")
                    NoReqBody
                    jsonResponse
                    (bearerHeader tokenConfig <> acceptLanguage))
    return $ parseEither pResponse body

pResponse :: Value -> Parser WeightGoal
pResponse = withObject "WeightGoalResponse" $ \v -> do
    obj <- v .: "goal"
    goalType <- obj .: "goalType"
    goalWeight <- obj .: "weight"
    startWeight <- obj .: "startWeight"
    return $ WeightGoal goalType goalWeight startWeight
