{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Main (main) where

import           Control.Exception (catch, throwIO)
import           Control.Monad (forM_)
import           Data.Aeson ((.:), Value, withArray, withObject)
import           Data.Aeson.Types (Parser, parseEither)
import           Data.Default.Class (def)
import           Data.Maybe (fromJust)
import           Data.Monoid ((<>))
import           Data.Text (Text)
import qualified Data.Text.Encoding as Text (encodeUtf8)
import qualified Data.Text.IO as Text (getLine, putStrLn)
import           Data.Time.Calendar (Day)
import           Data.Time.Clock (UTCTime(..), getCurrentTime)
import qualified Data.Vector as Vector (toList)
import           FitbitDemoApp
import           FitbitDemoLib
import           Network.HTTP.Types (unauthorized401)
import           Network.HTTP.Req
                    ( (/:)
                    , GET(..)
                    , NoReqBody(..)
                    , Scheme(..)
                    , Url
                    , header
                    , https
                    , jsonResponse
                    , oAuth2Bearer
                    , req
                    , responseBody
                    , runReq
                    )
import qualified Text.URI as URI (mkURI, render)
import           Text.URI.QQ (uri)

promptForAppConfig :: PromptForAppConfig
promptForAppConfig = do
    putStrLn "No Fitbit API configuration was found."
    putStr "Enter Fitbit client ID: "
    clientId <- ClientId <$> Text.getLine
    putStr "Enter Fitbit client secret: "
    clientSecret <- ClientSecret <$> Text.getLine
    return $ AppConfig (FitbitAPI clientId clientSecret)

promptForCallbackURI :: PromptForCallbackURI
promptForCallbackURI authUri' = do
    putStrLn "Open following link in browser:"
    Text.putStrLn $ URI.render authUri'
    putStr "Enter callback URI: "
    URI.mkURI =<< Text.getLine

fitbitApp :: OAuth2App
fitbitApp =
    OAuth2App
        [uri|https://api.fitbit.com/oauth2/token|]      -- tokenRequestUrl
        [uri|https://www.fitbit.com/oauth2/authorize|]  -- authUri

fitbitApiUrl :: Url 'Https
fitbitApiUrl = https "api.fitbit.com" /: "1"

foo :: Foo
foo authCode fitbitAPI = do
    result <- sendAccessToken fitbitApp (AccessTokenRequest fitbitAPI authCode)
    let (AccessTokenResponse at rt) = case result of
                                        Left e -> error e
                                        Right x -> x
    return $ TokenConfig at rt

refresh :: ClientId -> ClientSecret -> TokenConfig -> IO TokenConfig
refresh clientId clientSecret (TokenConfig _ refreshToken) = do
    result <- sendRefreshToken fitbitApp clientId clientSecret refreshToken
    let (RefreshTokenResponse at rt) = case result of
                                        Left e -> error e
                                        Right x -> x
    let newTokenConfig = TokenConfig at rt
    tokenConfigPath <- getTokenConfigPath
    encodeYAMLFile tokenConfigPath newTokenConfig
    return newTokenConfig

type APIAction a = TokenConfig -> IO a

withRefresh :: AppConfig -> TokenConfig -> APIAction a -> IO (a, TokenConfig)
withRefresh (AppConfig (FitbitAPI clientId clientSecret)) tokenConfig action =
    catch (action tokenConfig >>= \result -> return (result, tokenConfig)) $
        \e -> if hasResponseStatus e unauthorized401
                then do
                    newTokenConfig <- refresh clientId clientSecret tokenConfig
                    result <- action newTokenConfig
                    return (result, newTokenConfig)
                else throwIO e

getWeightGoal :: TokenConfig -> IO Value
getWeightGoal (TokenConfig (AccessToken at) _ ) = do
    let at' = Text.encodeUtf8 at
    responseBody <$> (runReq def $
        req GET
            (fitbitApiUrl /: "user" /: "-" /: "body" /: "log" /: "weight" /: "goal.json")
            NoReqBody
            jsonResponse
            (oAuth2Bearer at' <> header "Accept-Language" "en_US"))

data Period = OneDay | SevenDays | ThirtyDays | OneWeek | OneMonth | ThreeMonths | SixMonths | OneYear | Max

formatPeriod :: Period -> Text
formatPeriod OneDay = "1d"
formatPeriod SevenDays = "7d"
formatPeriod ThirtyDays = "30d"
formatPeriod OneWeek = "1w"
formatPeriod OneMonth = "1m"
formatPeriod ThreeMonths = "3m"
formatPeriod SixMonths = "6m"
formatPeriod OneYear = "1y"
formatPeriod Max = "max"

data TimeSeriesRange = Ending Day Period | Between Day Day

addTimeSeriesRangeToUrl :: TimeSeriesRange -> Url 'Https -> Url 'Https
addTimeSeriesRangeToUrl (Ending endDay period) u = u /: formatDay endDay /: formatPeriod period <> ".json"
addTimeSeriesRangeToUrl (Between startDay endDay) u = u /: formatDay startDay /: formatDay endDay <> ".json"

data WeightSample = WeightSample Day Text

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

getWeightTimeSeries :: TimeSeriesRange -> TokenConfig -> IO (Either String [WeightSample])
getWeightTimeSeries range (TokenConfig (AccessToken at) _) = do
    let at' = Text.encodeUtf8 at
    body <- responseBody <$> (runReq def $
                req GET
                    (addTimeSeriesRangeToUrl range (fitbitApiUrl /: "user" /: "-" /: "body" /: "weight" /: "date"))
                    NoReqBody
                    jsonResponse
                    (oAuth2Bearer at' <> header "Accept-Language" "en_US"))
    return $ parseEither pFoo body

main :: IO ()
main = do
    Just appConfig <- getAppConfig promptForAppConfig
    tc0 <- getTokenConfig fitbitApp foo promptForCallbackURI appConfig

    -- TODO: Refactor to use State etc.
    (weightGoal, tc1) <- withRefresh appConfig tc0 getWeightGoal
    print weightGoal

    t <- getCurrentTime
    let range = Ending (utctDay t) Max

    (weightTimeSeries, _) <- withRefresh appConfig tc1  (getWeightTimeSeries range)
    let Right ws = weightTimeSeries
    forM_ (take 5 ws) $ \(WeightSample day value) -> putStrLn $ show day ++ ": " ++ show value

    putStrLn "DONE"
