{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module FitbitAPI.WeightTimeSeries
    ( getWeightTimeSeries
    ) where

import           Data.Aeson ((.:), Value, withArray, withObject)
import           Data.Aeson.Types (Parser)
import           Data.Either (fromRight)
import           Data.Monoid ((<>))
import qualified Data.Vector as Vector (toList)
import           FitbitAPI.Period
import           FitbitAPI.TimeSeriesRange
import           FitbitAPI.WeightSample
import           Network.HTTP.Req ((/:), Scheme(..), Url)
import           Network.HTTP.Req.OAuth2 (App, OAuth2, oAuth2Get)
import           Util.Format (formatDay)
import           Util.Parser (parseDay, parseDouble)

getWeightTimeSeries :: App -> Url 'Https -> TimeSeriesRange -> OAuth2 [WeightSample]
getWeightTimeSeries app apiUrl range =
    oAuth2Get pResponse (buildUrl range (apiUrl /: "user" /: "-" /: "body" /: "weight" /: "date")) app

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
        <$> (fromRight (error "FAIL") . parseDay <$> v .: "dateTime") -- TODO: Error handling
        <*> (fromRight (error "FAIL") . parseDouble <$> v .: "value") -- TODO: Error handling
