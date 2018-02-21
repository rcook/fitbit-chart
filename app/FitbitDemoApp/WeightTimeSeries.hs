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
import qualified Data.Text.Encoding as Text (encodeUtf8)
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
                    , header
                    , jsonResponse
                    , oAuth2Bearer
                    , req
                    , responseBody
                    , runReq
                    )
import           OAuth2

pWeightSample :: Value -> Parser WeightSample
pWeightSample =
    withObject "WeightSample" $ \v -> WeightSample
        <$> (fromJust . parseDay <$> v .: "dateTime")
        <*> v .: "value"

pWeightSamples :: Value -> Parser [WeightSample]
pWeightSamples =
    withArray "[WeightSample]" $ mapM pWeightSample . Vector.toList

pFoo :: Value -> Parser [WeightSample]
pFoo = withObject "WeightTimeSeriesResponse" $ \v -> do
    obj <- v .: "body-weight"
    pWeightSamples obj

addTimeSeriesRangeToUrl :: TimeSeriesRange -> Url 'Https -> Url 'Https
addTimeSeriesRangeToUrl (Ending endDay period) u = u /: formatDay endDay /: formatPeriod period <> ".json"
addTimeSeriesRangeToUrl (Between startDay endDay) u = u /: formatDay startDay /: formatDay endDay <> ".json"

getWeightTimeSeries :: Url 'Https -> TimeSeriesRange -> TokenConfig -> IO (Either String [WeightSample])
getWeightTimeSeries fitbitApiUrl range (TokenConfig (AccessToken at) _) = do
    let at' = Text.encodeUtf8 at
    body <- responseBody <$> (runReq def $
                req GET
                    (addTimeSeriesRangeToUrl range (fitbitApiUrl /: "user" /: "-" /: "body" /: "weight" /: "date"))
                    NoReqBody
                    jsonResponse
                    (oAuth2Bearer at' <> header "Accept-Language" "en_US"))
    return $ parseEither pFoo body
