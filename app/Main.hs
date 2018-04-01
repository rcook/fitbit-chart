{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import           App
import           Control.Exception (throwIO)
import qualified Data.Aeson as Aeson (encode)
import           Data.List (sortOn)
import           Data.Monoid ((<>))
import           Data.Text (Text)
import qualified Data.Text as Text (splitOn)
import qualified Data.Text.IO as Text (getLine, putStrLn)
import           Data.Time.Clock (UTCTime(..), getCurrentTime)
import           Lib.AWS
import           Lib.Errors
import           Lib.FitbitAPI
import           Lib.Params
import           Lib.Storage
import           Lib.Util
import           Network.AWS.Easy (AWSConfig, connect)
import           Network.AWS.S3 (BucketName(..), ObjectKey(..))
import qualified Network.HTTP.Req.OAuth2 as OAuth2
                    ( AccessToken(..)
                    , App
                    , ClientId(..)
                    , ClientPair(..)
                    , ClientSecret(..)
                    , PromptForCallbackUri
                    , RefreshToken(..)
                    , TokenPair(..)
                    , UpdateTokenPair
                    , evalOAuth2
                    )
import           Options.Applicative
                    ( execParser
                    , fullDesc
                    , helper
                    , info
                    , progDesc
                    )
import           System.IO (hFlush, stdout)
import qualified Text.URI as URI (mkURI, render)

clientInfoName :: ParameterName
clientInfoName = ParameterName "/HLambda/FitbitAPI/ClientInfo"

tokenPairName :: ParameterName
tokenPairName = ParameterName "/HLambda/FitbitAPI/TokenPair"

bucketName :: BucketName
bucketName = BucketName "fitbit-demo"

objectKey :: ObjectKey
objectKey = ObjectKey "data.json"

promptForAppConfig :: AppConfigPrompt
promptForAppConfig = do
    putStrLn "No Fitbit API configuration was found."
    putStr "Enter Fitbit client ID: "
    hFlush stdout
    clientId <- OAuth2.ClientId <$> Text.getLine
    putStr "Enter Fitbit client secret: "
    hFlush stdout
    clientSecret <- OAuth2.ClientSecret <$> Text.getLine
    return $ AppConfig (OAuth2.ClientPair clientId clientSecret)

promptForCallbackUri :: OAuth2.PromptForCallbackUri
promptForCallbackUri authUri' = do
    putStrLn "Open following link in browser:"
    Text.putStrLn $ URI.render authUri'
    putStr "Enter callback URI including authorization code: "
    hFlush stdout
    URI.mkURI =<< Text.getLine

main :: IO ()
main = parseOptions >>= run
    where
        parseOptions = execParser $ info
            (helper <*> optionsParser)
            (fullDesc <> progDesc "Dump Richard's Fitbit data into a JSON file stored on S3")

run :: Options -> IO ()
run options = do
    logInfo "Start"
    conf <- getAWSConfigFromEnv

    logInfo "Reading from DynamoDB"
    weightSamples <- fetchWeightSamples conf options

    logInfo "Generate JSON file in S3"
    s3Session <- connect conf s3Service
    putBytes bucketName objectKey (Aeson.encode weightSamples) s3Session
    logInfo "End"

fetchWeightSamples :: AWSConfig -> Options -> IO [WeightSample]
fetchWeightSamples conf options = do
    logInfo "Querying Fitbit"
    (AppConfig clientPair, updateTokenPair, getTokenConfigAction) <- getConfig conf options
    let app = mkApp updateTokenPair clientPair
    TokenConfig tp <- getTokenConfigAction app

    t <- getCurrentTime
    weightTimeSeries <- OAuth2.evalOAuth2 tp $
        getWeightTimeSeries app fitbitApiUrl (Ending (utctDay t) Max)

    return $ sortOn (\(WeightSample day _) -> day) weightTimeSeries

getConfig :: AWSConfig -> Options -> IO (AppConfig, OAuth2.UpdateTokenPair, OAuth2.App -> IO TokenConfig)
getConfig _ (ConfigDir configDir) = do
    expandedConfigDir <- expandPath configDir
    appConfig <- exitOnFailure $ getAppConfig expandedConfigDir promptForAppConfig
    return
        ( appConfig
        , writeTokenConfig expandedConfigDir . TokenConfig
        , \app -> exitOnFailure $ getTokenConfig expandedConfigDir app promptForCallbackUri
        )
getConfig conf SSMProperties = do
    ssmSession <- connect conf ssmService
    cp <- getClientInfo ssmSession
    return
        ( AppConfig cp
        , \tp -> setTokenPair tp ssmSession
        , \_ -> TokenConfig <$> getTokenPair ssmSession
        )

------------------------

pPair :: Text -> Maybe (Text, Text)
pPair s =
    case Text.splitOn ";" s of
        s1 : s2 : [] -> return (s1, s2)
        _ -> Nothing

pHelper :: (a -> b -> c) -> (Text -> a) -> (Text -> b) -> Text -> Maybe c
pHelper c0 c1 c2 s = (\(s1, s2) -> c0 (c1 s1) (c2 s2)) <$> (pPair s)

pClientInfo :: Text -> Maybe OAuth2.ClientPair
pClientInfo = pHelper OAuth2.ClientPair OAuth2.ClientId OAuth2.ClientSecret

pTokenPair :: Text -> Maybe OAuth2.TokenPair
pTokenPair = pHelper OAuth2.TokenPair OAuth2.AccessToken OAuth2.RefreshToken

getPairHelper :: String -> ParameterName -> (Text -> Maybe a) -> SSMSession -> IO a
getPairHelper label parameterName p ssmSession = do
    s <- getSecureStringParameter parameterName ssmSession
    case p s of
        Nothing -> throwIO $ RuntimeError ("Could not parse " ++ label)
        Just result -> return result

getClientInfo :: SSMSession -> IO OAuth2.ClientPair
getClientInfo = getPairHelper "Fitbit API info" clientInfoName pClientInfo

getTokenPair :: SSMSession -> IO OAuth2.TokenPair
getTokenPair = getPairHelper "token pair" tokenPairName pTokenPair

setTokenPair :: OAuth2.TokenPair -> SSMSession -> IO ()
setTokenPair (OAuth2.TokenPair (OAuth2.AccessToken at) (OAuth2.RefreshToken rt)) ssmSession =
    let s = at <> ";" <> rt
    in setSecureStringParameter tokenPairName s ssmSession

