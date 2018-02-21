{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module FitbitDemoApp.WeightTimeSeries
    ( getWeightTimeSeries
    ) where

import           Data.Aeson ((.:), Value, withArray, withObject)
import           Data.Aeson.Types (Parser, parseEither)
import           Data.Default.Class (def)
import           Data.Maybe (fromJust)
import           Data.Monoid ((<>))
import qualified Data.Vector as Vector (toList)
import           FitbitDemoApp.Types
import           FitbitDemoApp.Util
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

getWeightTimeSeries :: Url 'Https -> TimeSeriesRange -> TokenConfig -> IO (Either String [WeightSample])
getWeightTimeSeries fitbitApiUrl range tokenConfig = do
    body <- responseBody <$> (runReq def $
                req GET
                    (buildUrl range (fitbitApiUrl /: "user" /: "-" /: "body" /: "weight" /: "date"))
                    NoReqBody
                    jsonResponse
                    (bearerHeader tokenConfig <> acceptLanguage))
    return $ parseEither pResponse body

buildUrl :: TimeSeriesRange -> Url 'Https -> Url 'Https
buildUrl (Ending endDay period) u = u /: formatDay endDay /: formatPeriod period <> ".json"
buildUrl (Between startDay endDay) u = u /: formatDay startDay /: formatDay endDay <> ".json"

pResponse :: Value -> Parser [WeightSample]
pResponse = withObject "WeightTimeSeriesResponse" $ \v -> do
    obj <- v .: "body-weight"
    pWeightSamples obj

pWeightSamples :: Value -> Parser [WeightSample]
pWeightSamples =
    withArray "[WeightSample]" $ mapM pWeightSample . Vector.toList

pWeightSample :: Value -> Parser WeightSample
pWeightSample =
    withObject "WeightSample" $ \v -> WeightSample
        <$> (fromJust . parseDay <$> v .: "dateTime")
        <*> v .: "value"
