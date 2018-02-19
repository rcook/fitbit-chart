{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Main (main) where

import           Control.Exception (catch, throwIO)
import           Data.Aeson ((.:), FromJSON(..), Value, withObject)
import           Data.Aeson.Types (parseEither)
import qualified Data.ByteString as ByteString (append)
import           Data.Default.Class (def)
import           Data.Monoid ((<>))
import           Data.Text (Text)
import qualified Data.Text.Encoding as Text (encodeUtf8)
import qualified Data.Text.IO as Text (getLine, putStrLn)
import           FitbitDemoApp
import           FitbitDemoLib
import           Network.HTTP.Client (HttpException(..), HttpExceptionContent(..), responseStatus)
import           Network.HTTP.Types (unauthorized401)
import           Network.HTTP.Req
                    ( (/:)
                    , (=:)
                    , GET(..)
                    , HttpException(..)
                    , NoReqBody(..)
                    , POST(..)
                    , ReqBodyUrlEnc(..)
                    , header
                    , https
                    , jsonResponse
                    , oAuth2Bearer
                    , req
                    , responseBody
                    , runReq
                    )
import           Network.HTTP.Req.Url.Extra (toUrlHttps)
import           System.Directory (doesFileExist)
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
promptForCallbackURI authUri = do
    putStrLn "Open following link in browser:"
    Text.putStrLn $ URI.render authUri
    putStr "Enter callback URI: "
    URI.mkURI =<< Text.getLine

foo :: Foo
foo url authCode (FitbitAPI clientId clientSecret) = do
    result <- sendAccessTokenRequest url authCode clientId clientSecret
    let (AccessTokenResponse at rt) = case result of
                                        Left e -> error e
                                        Right x -> x
    return $ TokenConfig at rt

-- TODO: Move to Config
readTokenConfig :: Foo -> PromptForCallbackURI -> AppConfig -> IO TokenConfig
readTokenConfig f prompt (AppConfig fitbitAPI@(FitbitAPI clientId _)) = do
    authCode <- getAuthCode clientId prompt
    let Just (url, _) = toUrlHttps [uri|https://api.fitbit.com/oauth2/token|]
    tokenConfig <- f url authCode fitbitAPI
    tokenConfigPath <- getTokenConfigPath
    encodeYAMLFile tokenConfigPath tokenConfig
    return tokenConfig

-- TODO: Move to Config
myGetTokenConfig :: Foo -> PromptForCallbackURI -> AppConfig -> IO TokenConfig
myGetTokenConfig f prompt config = do
    tokenConfigPath <- getTokenConfigPath
    tokenConfigExists <- doesFileExist tokenConfigPath
    if tokenConfigExists
        then do
            Just tokenConfig <- decodeYAMLFile tokenConfigPath
            return tokenConfig
        else readTokenConfig f prompt config

data RefreshTokenResponse = RefreshTokenResponse AccessToken RefreshToken deriving Show

instance FromJSON RefreshTokenResponse where
    parseJSON =
        withObject "RefreshTokenResponse" $ \v -> RefreshTokenResponse
            <$> (AccessToken <$> v .: "access_token")
            <*> (RefreshToken <$> v .: "refresh_token")

refreshAndInvoke :: ClientId -> ClientSecret -> TokenConfig -> IO a -> IO a
refreshAndInvoke clientId clientSecret (TokenConfig accessToken (RefreshToken rt)) action = do
    let url = https "api.fitbit.com" /: "oauth2" /: "token"
    let opts = header "Authorization" (ByteString.append "Basic " (encodeClientAuth clientId clientSecret))
        formBody = "grant_type" =: ("refresh_token" :: Text) <> "refresh_token" =: rt <> "expires_in" =: ("3600" :: Text)
    body <- runReq def $ responseBody <$> req POST url (ReqBodyUrlEnc formBody) jsonResponse opts
    putStrLn "REFRESH AND REVOKE"
    let RefreshTokenResponse at rt' = case parseEither parseJSON body of
                                        Left e -> error $ "BAIL: " ++ e
                                        Right x -> x
        newTokenConfig = TokenConfig at rt'
        AccessToken at0 = accessToken
        AccessToken at1 = at
    putStrLn $ "accessToken stayed the same: " ++ show (at0 == at1)
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
    tokenConfig@(TokenConfig accessToken _) <- myGetTokenConfig foo promptForCallbackURI config
    weightGoal <- withRefresh clientId clientSecret tokenConfig $ getWeightGoal accessToken
    print weightGoal

    putStrLn "DONE"
