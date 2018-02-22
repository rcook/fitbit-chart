{-# LANGUAGE OverloadedStrings #-}

module FitbitDemoApp.Config
    ( PromptForAppConfig
    , TokenPairWrapper(..)
    , getAppConfig
    , getTokenPair
    , getTokenPairPath
    ) where

import           Data.Aeson
                    ( (.=)
                    , (.:)
                    , FromJSON(..)
                    , ToJSON(..)
                    , object
                    , withObject
                    )
import           FitbitDemoLib
import qualified Network.HTTP.Req.OAuth2 as OAuth2
                    ( AccessToken(..)
                    , AccessTokenRequest(..)
                    , AccessTokenResponse(..)
                    , App
                    , AuthCode
                    , ClientPair(..)
                    , PromptForCallbackURI
                    , RefreshToken(..)
                    , TokenPair(..)
                    , fetchAccessToken
                    , getAuthCode
                    )
import           System.Directory (createDirectoryIfMissing, doesFileExist, getHomeDirectory)
import           System.FilePath ((</>), takeDirectory)

type PromptForAppConfig = IO AppConfig

-- Wrap with a newtype to avoid creating an orphan instance of FromJSON/ToJSON for TokenPair
-- TODO: There's probably a more elegant way of serializing/deserializing TokenPair as YAML
-- where we can use the two functions directly
newtype TokenPairWrapper = TokenPairWrapper OAuth2.TokenPair

instance FromJSON TokenPairWrapper where
    parseJSON =
        withObject "TokenPair" $ \v -> (TokenPairWrapper .) . OAuth2.TokenPair
            <$> (OAuth2.AccessToken <$> v .: "access-token")
            <*> (OAuth2.RefreshToken <$> v .: "refresh-token")

instance ToJSON TokenPairWrapper where
    toJSON (TokenPairWrapper (OAuth2.TokenPair (OAuth2.AccessToken at) (OAuth2.RefreshToken rt))) =
        object
            [ "access-token" .= at
            , "refresh-token" .= rt
            ]

configDir :: FilePath
configDir = ".fitbit-demo"

getAppConfigPath :: IO FilePath
getAppConfigPath = do
    homeDir <- getHomeDirectory
    return $ homeDir </> configDir </> "config.yaml"

getTokenPairPath :: IO FilePath
getTokenPairPath = do
    homeDir <- getHomeDirectory
    return $ homeDir </> configDir </> "token.yaml"

newAppConfig :: PromptForAppConfig -> FilePath -> IO AppConfig
newAppConfig prompt p = do
    config <- prompt
    createDirectoryIfMissing True (takeDirectory p)
    encodeYAMLFile p config
    return config

getAppConfig :: PromptForAppConfig -> IO (Maybe AppConfig)
getAppConfig prompt = do
    appConfigPath <- getAppConfigPath
    exists <- doesFileExist appConfigPath
    if exists
        then decodeYAMLFile appConfigPath
        else Just <$> newAppConfig prompt appConfigPath

readTokenPair :: OAuth2.App -> OAuth2.ClientPair -> OAuth2.PromptForCallbackURI -> IO OAuth2.TokenPair
readTokenPair app clientPair@(OAuth2.ClientPair clientId _) prompt = do
    authCode <- OAuth2.getAuthCode app clientId prompt
    tokenPair <- foo app authCode clientPair
    tokenPairPath <- getTokenPairPath
    -- TODO: Figure out how to do this without a ToJSON instance
    encodeYAMLFile tokenPairPath (TokenPairWrapper tokenPair)
    return tokenPair

-- | Gets token pair
--
-- If a token pair exists in the token pair configuration file, read
-- it from the file and return that. Otherwise
getTokenPair :: OAuth2.App -> OAuth2.ClientPair -> OAuth2.PromptForCallbackURI -> IO OAuth2.TokenPair
getTokenPair app clientPair prompt = do
    tokenPairPath <- getTokenPairPath
    exists <- doesFileExist tokenPairPath
    if exists
        then do
            Just (TokenPairWrapper tokenPair) <- decodeYAMLFile tokenPairPath -- TODO: Error handling!
            return tokenPair
        else readTokenPair app clientPair prompt

foo :: OAuth2.App -> OAuth2.AuthCode -> OAuth2.ClientPair -> IO OAuth2.TokenPair
foo app authCode clientPair = do
    result <- OAuth2.fetchAccessToken app (OAuth2.AccessTokenRequest clientPair authCode)
    let (OAuth2.AccessTokenResponse tokenPair) = case result of
                                                Left e -> error e
                                                Right x -> x
    return tokenPair
        