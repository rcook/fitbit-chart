{-# LANGUAGE OverloadedStrings #-}

module FitbitDemoApp.TokenConfig
    ( TokenConfig(..)
    , getTokenPair
    , getTokenPairPath
    , writeTokenPair
    ) where

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

getTokenPairPath :: FilePath -> IO FilePath
getTokenPairPath configDir = do
    homeDir <- getHomeDirectory
    return $ homeDir </> configDir </> "token.yaml"

readTokenPair :: FilePath -> OAuth2.App -> OAuth2.ClientPair -> OAuth2.PromptForCallbackURI -> IO OAuth2.TokenPair
readTokenPair configDir app clientPair@(OAuth2.ClientPair clientId _) prompt = do
    authCode <- OAuth2.getAuthCode app clientId prompt
    tokenPair <- foo app authCode clientPair
    writeTokenPair configDir tokenPair
    return tokenPair

writeTokenPair :: FilePath -> OAuth2.TokenPair -> IO ()
writeTokenPair configDir tokenPair = do
    path <- getTokenPairPath configDir
    createDirectoryIfMissing True (takeDirectory path)
    encodeYAMLFile path (TokenConfig tokenPair)

-- | Gets token pair
--
-- If a token pair exists in the token pair configuration file, read
-- it from the file and return that. Otherwise
getTokenPair :: FilePath -> OAuth2.App -> OAuth2.ClientPair -> OAuth2.PromptForCallbackURI -> IO OAuth2.TokenPair
getTokenPair configDir app clientPair prompt = do
    path <- getTokenPairPath configDir
    exists <- doesFileExist path
    if exists
        then do
            Just (TokenConfig tokenPair) <- decodeYAMLFile path -- TODO: Error handling!
            return tokenPair
        else readTokenPair configDir app clientPair prompt

foo :: OAuth2.App -> OAuth2.AuthCode -> OAuth2.ClientPair -> IO OAuth2.TokenPair
foo app authCode clientPair = do
    result <- OAuth2.fetchAccessToken app (OAuth2.AccessTokenRequest clientPair authCode)
    let (OAuth2.AccessTokenResponse tokenPair) = case result of
                                                Left e -> error e
                                                Right x -> x
    return tokenPair
        