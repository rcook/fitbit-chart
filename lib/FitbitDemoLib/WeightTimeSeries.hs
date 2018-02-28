{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module FitbitDemoLib.WeightTimeSeries
    ( getWeightTimeSeries
    ) where

import           Data.Aeson ((.:), Value, withArray, withObject)
import           Data.Aeson.Types (Parser, parseEither)
import           Data.Either (fromRight)
import           Data.Monoid ((<>))
import qualified Data.Vector as Vector (toList)
import           FitbitDemoLib.DateTime
import           FitbitDemoLib.OAuth2Helper
import           FitbitDemoLib.Parser
import           FitbitDemoLib.Types
import           FitbitDemoLib.Util
import           Network.HTTP.Req ((/:), Scheme(..), Url)
import qualified Network.HTTP.Req.OAuth2 as OAuth2 (TokenPair(..))

getWeightTimeSeries :: TimeSeriesRange -> APIAction [WeightSample]
getWeightTimeSeries range apiUrl (OAuth2.TokenPair accessToken _) =
    parseEither pResponse
        <$> oAuth2Get (buildUrl range (apiUrl /: "user" /: "-" /: "body" /: "weight" /: "date")) accessToken
    
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
