{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module FitbitDemoApp.WeightGoal
    ( getWeightGoal
    ) where

import           Data.Aeson (Value)
import           Data.Default.Class (def)
import           Data.Monoid ((<>))
import           FitbitDemoLib
import           Network.HTTP.Req
                    ( (/:)
                    , GET(..)
                    , NoReqBody(..)
                    , Scheme(..)
                    , Url
                    , jsonResponse
                    , req
                    , responseBody
                    , runReq
                    )

getWeightGoal :: Url 'Https -> TokenConfig -> IO Value
getWeightGoal fitbitApiUrl tokenConfig = do
    responseBody <$> (runReq def $
        req GET
            (fitbitApiUrl /: "user" /: "-" /: "body" /: "log" /: "weight" /: "goal.json")
            NoReqBody
            jsonResponse
            (bearerHeader tokenConfig <> acceptLanguage))
