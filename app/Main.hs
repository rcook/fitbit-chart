{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import           App
import           CommandLine
import           Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.Aeson as Aeson (encode)
import           Data.List (sortOn)
import           Data.Monoid ((<>))
import qualified Data.Text.IO as Text (getLine, putStrLn)
import           Data.Time.Clock (UTCTime(..), getCurrentTime)
import           Lib.AWS
import           Lib.FitbitAPI
import           Lib.Params
import           Lib.Storage
import           Lib.Util
import           Network.AWS.Easy (AWSConfig, connect)
import           Network.AWS.S3 (BucketName(..), ObjectKey(..))
import qualified Network.HTTP.Req.OAuth2 as OAuth2
                    ( App
                    , ClientId(..)
                    , ClientPair(..)
                    , ClientSecret(..)
                    , PromptForCallbackUri
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
import           System.Exit (exitFailure)
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

exitOnFailure :: (MonadIO m) => m (Either String a) -> m a
exitOnFailure action = do
    result <- action
    case result of
        Left e -> liftIO (putStrLn e >> exitFailure)
        Right x -> return x

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
    cp <- getClientInfo clientInfoName ssmSession
    return
        ( AppConfig cp
        , \tp -> setTokenPair tokenPairName tp ssmSession
        , \_ -> TokenConfig <$> getTokenPair tokenPairName ssmSession
        )
