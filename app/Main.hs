{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Main (main) where

import           Control.Exception (catch, throwIO)
import           Control.Monad (forM_)
import           Data.Monoid ((<>))
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
import qualified Network.HTTP.Req.OAuth2 as OAuth2
                    ( AccessTokenRequest(..)
                    , AccessTokenResponse(..)
                    , App(..)
                    , ClientId(..)
                    , ClientSecret(..)
                    , PromptForCallbackURI
                    , RefreshTokenRequest(..)
                    , RefreshTokenResponse(..)
                    , TokenPair(..)
                    , fetchAccessToken
                    , fetchRefreshToken
                    )
import           Text.Printf (printf)
import qualified Text.URI as URI (mkURI, render)
import           Text.URI.QQ (uri)

promptForAppConfig :: PromptForAppConfig
promptForAppConfig = do
    putStrLn "No Fitbit API configuration was found."
    putStr "Enter Fitbit client ID: "
    clientId <- OAuth2.ClientId <$> Text.getLine
    putStr "Enter Fitbit client secret: "
    clientSecret <- OAuth2.ClientSecret <$> Text.getLine
    return $ AppConfig (FitbitAPI clientId clientSecret)

promptForCallbackURI :: OAuth2.PromptForCallbackURI
promptForCallbackURI authUri' = do
    putStrLn "Open following link in browser:"
    Text.putStrLn $ URI.render authUri'
    putStr "Enter callback URI: "
    URI.mkURI =<< Text.getLine

fitbitApp :: OAuth2.App
fitbitApp =
    OAuth2.App
        [uri|https://www.fitbit.com/oauth2/authorize|]  -- authUri
        [uri|https://api.fitbit.com/oauth2/token|]      -- tokenUri

fitbitApiUrl :: Url 'Https
fitbitApiUrl = https "api.fitbit.com" /: "1"

foo :: Foo
foo authCode (FitbitAPI clientId clientSecret) = do
    result <- OAuth2.fetchAccessToken fitbitApp (OAuth2.AccessTokenRequest clientId clientSecret authCode)
    let (OAuth2.AccessTokenResponse at rt) = case result of
                                                Left e -> error e
                                                Right x -> x
    return $ OAuth2.TokenPair at rt

formatDouble :: Double -> String
formatDouble = printf "%.1f"

main :: IO ()
main = do
    Just appConfig <- getAppConfig promptForAppConfig
    tp0 <- getTokenPair fitbitApp foo promptForCallbackURI appConfig

    -- TODO: Refactor to use State etc.
    (Right weightGoal, tp1) <- withRefresh fitbitApp fitbitApiUrl appConfig tp0 getWeightGoal
    Text.putStrLn $ "Goal type: " <> goalType weightGoal
    putStrLn $ "Goal weight: " ++ formatDouble (goalWeight weightGoal) ++ " lbs"
    putStrLn $ "Start weight: " ++ formatDouble (startWeight weightGoal) ++ " lbs"

    t <- getCurrentTime
    let range = Ending (utctDay t) Max

    (weightTimeSeries, _) <- withRefresh fitbitApp fitbitApiUrl appConfig tp1  (getWeightTimeSeries range)
    let Right ws = weightTimeSeries
    forM_ (take 5 ws) $ \(WeightSample day value) ->
        putStrLn $ show day ++ ": " ++ formatDouble value ++ " lbs"

    putStrLn "DONE"
