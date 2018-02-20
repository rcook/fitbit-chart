{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Main (main) where

import           Control.Exception (catch, throwIO)
import           Data.Aeson (Value)
import           Data.Default.Class (def)
import           Data.Monoid ((<>))
import qualified Data.Text.Encoding as Text (encodeUtf8)
import qualified Data.Text.IO as Text (getLine, putStrLn)
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

refresh :: ClientId -> ClientSecret -> TokenConfig -> IO ()
refresh clientId clientSecret (TokenConfig _ refreshToken) = do
    result <- sendRefreshToken fitbitApp clientId clientSecret refreshToken
    let (RefreshTokenResponse at rt) = case result of
                                        Left e -> error e
                                        Right x -> x
    let newTokenConfig = TokenConfig at rt
    tokenConfigPath <- getTokenConfigPath
    encodeYAMLFile tokenConfigPath newTokenConfig

withRefresh :: ClientId -> ClientSecret -> TokenConfig -> IO a -> IO a
withRefresh clientId clientSecret tokenConfig action =
    catch action $ \e -> if hasResponseStatus e unauthorized401
                            then refresh clientId clientSecret tokenConfig >> action
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

main :: IO ()
main = do
    Just config@(AppConfig (FitbitAPI clientId clientSecret)) <- getAppConfig promptForAppConfig
    tokenConfig@(TokenConfig accessToken _) <- getTokenConfig fitbitApp foo promptForCallbackURI config
    weightGoal <- withRefresh clientId clientSecret tokenConfig $ getWeightGoal accessToken
    print weightGoal

    putStrLn "DONE"
