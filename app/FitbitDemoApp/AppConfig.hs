{-# LANGUAGE OverloadedStrings #-}

module FitbitDemoApp.AppConfig
    ( AppConfig(..)
    , AppConfigPrompt
    , getAppConfig
    ) where

import           Data.Aeson ((.:), (.=), FromJSON(..), ToJSON(..), object, withObject)
import           FitbitDemoLib.IO (decodeYAMLFile, encodeYAMLFile)
import qualified Network.HTTP.Req.OAuth2 as OAuth2
                    ( ClientId(..)
                    , ClientPair(..)
                    , ClientSecret(..)
                    )
import           System.Directory (createDirectoryIfMissing, doesFileExist, getHomeDirectory)
import           System.FilePath ((</>), takeDirectory)

type AppConfigPrompt = IO AppConfig

-- | App configuration
data AppConfig = AppConfig OAuth2.ClientPair deriving (Eq, Show)

-- | 'FromJSON' instance for deserializing app configuration from YAML
instance FromJSON AppConfig where
    parseJSON =
        withObject "AppConfig" $ \v -> do
            fitbitApi <- v .: "fitbit-api"
            clientId <- OAuth2.ClientId <$> fitbitApi .: "client-id"
            clientSecret <- OAuth2.ClientSecret <$> fitbitApi .: "client-secret"
            let clientPair = OAuth2.ClientPair clientId clientSecret
            return $ AppConfig clientPair

-- | 'ToJSON' instance for serializing app configuration to YAML
instance ToJSON AppConfig where
    toJSON (AppConfig (OAuth2.ClientPair (OAuth2.ClientId cid) (OAuth2.ClientSecret cs))) =
        object
            [ "fitbit-api" .= object
                                [ "client-id" .= cid
                                , "client-secret" .= cs
                                ]
            ]

getAppConfig :: FilePath -> AppConfigPrompt -> IO (Maybe AppConfig)
getAppConfig configDir prompt = do
    path <- getAppConfigPath configDir
    exists <- doesFileExist path
    if exists
        then decodeYAMLFile path
        else Just <$> mkAppConfig prompt path

getAppConfigPath :: FilePath -> IO FilePath
getAppConfigPath configDir = do
    homeDir <- getHomeDirectory
    return $ homeDir </> configDir </> "config.yaml"

mkAppConfig :: AppConfigPrompt -> FilePath -> IO AppConfig
mkAppConfig prompt path = do
    config <- prompt
    createDirectoryIfMissing True (takeDirectory path)
    encodeYAMLFile path config
    return config
