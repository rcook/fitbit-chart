{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Main (main) where

import           Control.Exception (catch, throwIO)
import           Control.Monad (forM_)
import qualified Data.Text.IO as Text (getLine, putStrLn)
import           Data.Time.Clock (UTCTime(..), getCurrentTime)
import           FitbitDemoApp
import           FitbitDemoLib
import           Network.HTTP.Types (unauthorized401)
import           Network.HTTP.Req
                    ( (/:)
                    , Scheme(..)
                    , Url
                    , https
                    )
import           OAuth2
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

fitbitApp :: App
fitbitApp =
    App
        [uri|https://www.fitbit.com/oauth2/authorize|]  -- authUri
        [uri|https://api.fitbit.com/oauth2/token|]      -- tokenUri

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

withRefresh :: AppConfig -> TokenConfig -> APIAction a -> IO (a, TokenConfig)
withRefresh (AppConfig (FitbitAPI clientId clientSecret)) tokenConfig action =
    catch (action tokenConfig >>= \result -> return (result, tokenConfig)) $
        \e -> if hasResponseStatus e unauthorized401
                then do
                    newTokenConfig <- refresh clientId clientSecret tokenConfig
                    result <- action newTokenConfig
                    return (result, newTokenConfig)
                else throwIO e

main :: IO ()
main = do
    Just appConfig <- getAppConfig promptForAppConfig
    tc0 <- getTokenConfig fitbitApp foo promptForCallbackURI appConfig

    -- TODO: Refactor to use State etc.
    (weightGoal, tc1) <- withRefresh appConfig tc0 (getWeightGoal fitbitApiUrl)
    print weightGoal

    t <- getCurrentTime
    let range = Ending (utctDay t) Max

    (weightTimeSeries, _) <- withRefresh appConfig tc1  (getWeightTimeSeries fitbitApiUrl range)
    let Right ws = weightTimeSeries
    forM_ (take 5 ws) $ \(WeightSample day value) ->
        putStrLn $ show day ++ ": " ++ show value

    putStrLn "DONE"
