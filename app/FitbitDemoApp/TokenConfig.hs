{-# LANGUAGE OverloadedStrings #-}

module FitbitDemoApp.TokenConfig
    ( TokenConfig(..)
    , getTokenConfig
    , writeTokenConfig
    ) where

import           Control.Error.Util (note)
import           Data.Aeson
                    ( (.=)
                    , (.:)
                    , FromJSON(..)
                    , ToJSON(..)
                    , object
                    , withObject
                    )
import           FitbitDemoLib.IO (decodeYAMLFile, encodeYAMLFile)
import qualified Network.HTTP.Req.OAuth2 as OAuth2
                    ( AccessToken(..)
                    , AccessTokenRequest(..)
                    , AccessTokenResponse(..)
                    , App(..)
                    , ClientPair(..)
                    , PromptForCallbackURI
                    , RefreshToken(..)
                    , TokenPair(..)
                    , fetchAccessToken
                    , getAuthCode
                    )
import           System.Directory (createDirectoryIfMissing, doesFileExist, getHomeDirectory)
import           System.FilePath ((</>), takeDirectory)

-- | Token configuration
data TokenConfig = TokenConfig OAuth2.TokenPair deriving Show

-- | 'FromJSON' instance for deserializing token configuration from YAML
instance FromJSON TokenConfig where
    parseJSON =
        withObject "TokenConfig" $ \v -> (TokenConfig .) . OAuth2.TokenPair
            <$> (OAuth2.AccessToken <$> v .: "access-token")
            <*> (OAuth2.RefreshToken <$> v .: "refresh-token")

-- | 'ToJSON' instance for serializing token configuration to YAML
instance ToJSON TokenConfig where
    toJSON (TokenConfig (OAuth2.TokenPair (OAuth2.AccessToken at) (OAuth2.RefreshToken rt))) =
        object
            [ "access-token" .= at
            , "refresh-token" .= rt
            ]

-- | Gets token configuration from file or via authorization code workflow
--
-- If a token pair exists in the token pair configuration file, read
-- it from the file and return that. Otherwise perform authorization code
-- workflow.
getTokenConfig :: FilePath -> OAuth2.App -> OAuth2.PromptForCallbackURI -> IO (Either String TokenConfig)
getTokenConfig configDir app prompt = do
    path <- getTokenConfigPath configDir
    exists <- doesFileExist path
    if exists
        then note ("Could not read token configuration from " ++ path) <$> decodeYAMLFile path
        else fetchTokenConfig configDir app prompt

-- | Writes token configuration to configuration file
writeTokenConfig :: FilePath -> TokenConfig -> IO ()
writeTokenConfig configDir tokenConfig = do
    path <- getTokenConfigPath configDir
    createDirectoryIfMissing True (takeDirectory path)
    encodeYAMLFile path tokenConfig

getTokenConfigPath :: FilePath -> IO FilePath
getTokenConfigPath configDir = do
    homeDir <- getHomeDirectory
    return $ homeDir </> configDir </> "token.yaml"

fetchTokenConfig :: FilePath -> OAuth2.App -> OAuth2.PromptForCallbackURI -> IO (Either String TokenConfig)
fetchTokenConfig configDir app@(OAuth2.App _ _ _ (OAuth2.ClientPair clientId _)) prompt = do
    authCode <- OAuth2.getAuthCode app clientId prompt
    result <- OAuth2.fetchAccessToken app (OAuth2.AccessTokenRequest authCode)
    case result of
        Right (OAuth2.AccessTokenResponse tokenPair) -> do
            let tokenConfig = TokenConfig tokenPair
            writeTokenConfig configDir tokenConfig
            return $ Right tokenConfig
        Left e -> return $ Left e
