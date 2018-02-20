{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import           Control.Exception (catch, throwIO)
import           Data.Aeson (Value)
import           Data.Default.Class (def)
import           Data.Monoid ((<>))
import qualified Data.Text.Encoding as Text (encodeUtf8)
import qualified Data.Text.IO as Text (getLine, putStrLn)
import           FitbitDemoApp
import           FitbitDemoLib
import           Network.HTTP.Client (HttpException(..), HttpExceptionContent(..), responseStatus)
import           Network.HTTP.Types (unauthorized401)
import           Network.HTTP.Req
                    ( (/:)
                    , GET(..)
                    , HttpException(..)
                    , NoReqBody(..)
                    , header
                    , https
                    , jsonResponse
                    , oAuth2Bearer
                    , req
                    , responseBody
                    , runReq
                    )
import qualified Text.URI as URI (mkURI, render)

promptForAppConfig :: PromptForAppConfig
promptForAppConfig = do
    putStrLn "No Fitbit API configuration was found."
    putStr "Enter Fitbit client ID: "
    clientId <- ClientId <$> Text.getLine
    putStr "Enter Fitbit client secret: "
    clientSecret <- ClientSecret <$> Text.getLine
    return $ AppConfig (FitbitAPI clientId clientSecret)

promptForCallbackURI :: PromptForCallbackURI
promptForCallbackURI authUri = do
    putStrLn "Open following link in browser:"
    Text.putStrLn $ URI.render authUri
    putStr "Enter callback URI: "
    URI.mkURI =<< Text.getLine

foo :: Foo
foo url authCode fitbitAPI = do
    result <- sendAccessToken url (AccessTokenRequest fitbitAPI authCode)
    let (AccessTokenResponse at rt) = case result of
                                        Left e -> error e
                                        Right x -> x
    return $ TokenConfig at rt

refreshAndInvoke :: ClientId -> ClientSecret -> TokenConfig -> IO a -> IO a
refreshAndInvoke clientId clientSecret (TokenConfig _ refreshToken) action = do
    result <- sendRefreshToken clientId clientSecret refreshToken
    let (RefreshTokenResponse at rt) = case result of
                                        Left e -> error e
                                        Right x -> x
    let newTokenConfig = TokenConfig at rt
    tokenConfigPath <- getTokenConfigPath
    encodeYAMLFile tokenConfigPath newTokenConfig
    action

withRefresh :: ClientId -> ClientSecret -> TokenConfig -> IO a -> IO a
withRefresh clientId clientSecret tokenConfig action = do
    result <- catch action $ \e ->
                case e of
                    VanillaHttpException (HttpExceptionRequest _ (StatusCodeException response _)) ->
                        if responseStatus response == unauthorized401
                            then refreshAndInvoke clientId clientSecret tokenConfig action
                            else throwIO e
                    _ -> throwIO e
    return result

getWeightGoal :: AccessToken -> IO Value
getWeightGoal (AccessToken at) = do
    let at' = Text.encodeUtf8 at
    responseBody <$> (runReq def $
        req GET
            (https "api.fitbit.com" /: "1" /: "user" /: "-" /: "body" /: "log" /: "weight" /: "goal.json")
            NoReqBody
            jsonResponse
            (oAuth2Bearer at' <> header "Accept-Language" "en_US"))

main :: IO ()
main = do
    Just config@(AppConfig (FitbitAPI clientId clientSecret)) <- getAppConfig promptForAppConfig
    tokenConfig@(TokenConfig accessToken _) <- getTokenConfig foo promptForCallbackURI config
    weightGoal <- withRefresh clientId clientSecret tokenConfig $ getWeightGoal accessToken
    print weightGoal

    putStrLn "DONE"
