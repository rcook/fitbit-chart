{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Main (main) where

import           Control.Exception (catch, throwIO)
import           Data.Aeson ((.:), Value, withObject)
import           Data.Aeson.Types (Parser)
import           Data.Default.Class (def)
import           Data.Monoid ((<>))
import           Data.Text (Text)
import qualified Data.Text.Encoding as Text (encodeUtf8)
import qualified Data.Text.IO as Text (getLine, putStrLn)
import           Data.Time.Calendar (Day)
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

fitbitUrl :: Url 'Https
fitbitUrl = https "api.fitbit.com" /: "1"

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

withRefresh :: ClientId -> ClientSecret -> TokenConfig -> IO a -> IO (a, TokenConfig)
withRefresh clientId clientSecret tokenConfig action =
    catch (action >>= \result -> return (result, tokenConfig)) $
        \e -> if hasResponseStatus e unauthorized401
                then do
                    newTokenConfig <- refresh clientId clientSecret tokenConfig
                    result <- action
                    return (result, newTokenConfig)
                else throwIO e

getWeightGoal :: AccessToken -> IO Value
getWeightGoal (AccessToken at) = do
    let at' = Text.encodeUtf8 at
    responseBody <$> (runReq def $
        req GET
            (fitbitUrl /: "user" /: "-" /: "body" /: "log" /: "weight" /: "goal.json")
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

data WeightSample = WeightSample Day Text

pWeightSample :: Value -> Parser WeightSample
pWeightSample =
    withObject "AccessTokenResponse" $ \v -> WeightSample
        <$> (parseDay <$> v .: "dateTime")
        <*> v .: "value"

getWeightTimeSeries :: AccessToken -> Day -> Period -> IO Value
getWeightTimeSeries (AccessToken at) startDay period = do
    let at' = Text.encodeUtf8 at
    responseBody <$> (runReq def $
        req GET
            (fitbitUrl /: "user" /: "-" /: "body" /: "weight" /: "date" /: formatDay startDay /: formatPeriod period <> ".json")
            NoReqBody
            jsonResponse
            (oAuth2Bearer at' <> header "Accept-Language" "en_US"))

main :: IO ()
main = do
    Just config@(AppConfig (FitbitAPI clientId clientSecret)) <- getAppConfig promptForAppConfig
    tokenConfig@(TokenConfig accessToken _) <- getTokenConfig fitbitApp foo promptForCallbackURI config

    let at0 = accessToken
        tc0 = tokenConfig

    -- TODO: Refactor to use State etc.
    (weightGoal, tc1@(TokenConfig at1 _)) <- withRefresh clientId clientSecret tc0 $ getWeightGoal at0
    print weightGoal

    let d = mkDay 2014 9 8
    (weightTimeSeries, _) <- withRefresh clientId clientSecret tc1 $ getWeightTimeSeries at1 d Max
    print weightTimeSeries

    putStrLn "DONE"
