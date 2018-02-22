{-# LANGUAGE OverloadedStrings #-}

module FitbitDemoApp.Config
    ( Foo
    , PromptForAppConfig
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
                    , App
                    , AuthCode
                    , PromptForCallbackURI
                    , RefreshToken(..)
                    , TokenPair(..)
                    , getAuthCode
                    )
import           System.Directory (createDirectoryIfMissing, doesFileExist, getHomeDirectory)
import           System.FilePath ((</>), takeDirectory)

type PromptForAppConfig = IO AppConfig
type Foo = OAuth2.AuthCode -> FitbitAPI -> IO OAuth2.TokenPair

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
    appConfigExists <- doesFileExist appConfigPath
    if appConfigExists
        then decodeYAMLFile appConfigPath
        else Just <$> newAppConfig prompt appConfigPath

readTokenPair :: OAuth2.App -> Foo -> OAuth2.PromptForCallbackURI -> AppConfig -> IO OAuth2.TokenPair
readTokenPair app f prompt (AppConfig fitbitAPI@(FitbitAPI clientId _)) = do
    authCode <- OAuth2.getAuthCode app clientId prompt
    tokenPair <- f authCode fitbitAPI
    tokenPairPath <- getTokenPairPath
    -- TODO: Figure out how to do this without a ToJSON instance
    encodeYAMLFile tokenPairPath (TokenPairWrapper tokenPair)
    return tokenPair

getTokenPair :: OAuth2.App -> Foo -> OAuth2.PromptForCallbackURI -> AppConfig -> IO OAuth2.TokenPair
getTokenPair app f prompt config = do
    tokenPairPath <- getTokenPairPath
    tokenPairExists <- doesFileExist tokenPairPath
    if tokenPairExists
        then do
            Just (TokenPairWrapper tokenPair) <- decodeYAMLFile tokenPairPath -- TODO: Error handling!
            return tokenPair
        else readTokenPair app f prompt config
