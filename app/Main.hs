{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import           Control.Lens ((&), (.~))
import           Control.Monad (void)
import qualified Data.Aeson as Aeson (encode)
import           Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as ByteString (writeFile)
import           Data.Monoid ((<>))
import qualified Data.Text as Text (pack)
import qualified Data.Text.IO as Text (getLine, putStrLn)
import           Data.Time.Clock (UTCTime(..), getCurrentTime)
import           FitbitDemoApp
import           FitbitDemoLib
import           Network.AWS
                    ( Credentials(..)
                    , Region(..)
                    , send
                    )
import           Network.AWS.Data.Body (RqBody(..), ToHashedBody(..))
import           Network.AWS.Easy
                    ( Endpoint(..)
                    , awsConfig
                    , awscCredentials
                    , connect
                    , withAWS
                    )
import           Network.AWS.S3
                    ( BucketName(..)
                    , ObjectKey(..)
                    , putObject
                    )
import qualified Network.HTTP.Req.OAuth2 as OAuth2
                    ( ClientId(..)
                    , ClientPair(..)
                    , ClientSecret(..)
                    , PromptForCallbackUri
                    , evalOAuth2
                    )
import           Options.Applicative
                    ( Parser
                    , execParser
                    , fullDesc
                    , helper
                    , info
                    , progDesc
                    )
import           System.Environment (getEnv)
import           System.IO (hFlush, stdout)
import qualified Text.URI as URI (mkURI, render)

data Options = Options

pOptions :: Parser Options
pOptions = pure Options

configDir :: FilePath
configDir = ".fitbit-demo"

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
            (helper <*> pOptions)
            (fullDesc <> progDesc "Dump Richard's Fitbit data into a CSV file")

tableName :: TableName
tableName = TableName "weight-samples"

run' :: Options -> IO ()
run' _ = do
    AppConfig clientPair <- exitOnFailure $ getAppConfig configDir promptForAppConfig

    let app = mkApp
                (writeTokenConfig configDir . TokenConfig)
                clientPair

    TokenConfig tp <- exitOnFailure $ getTokenConfig configDir app promptForCallbackUri

    {-
    t <- getCurrentTime
    (weightGoal, weightTimeSeries) <- OAuth2.evalOAuth2 tp $ (,)
        <$> getWeightGoal app fitbitApiUrl
        <*> getWeightTimeSeries app fitbitApiUrl (Ending (utctDay t) Max)
    Text.putStrLn $ "Goal type: " <> goalType weightGoal
    putStrLn $ "Goal weight: " ++ formatDouble (goalWeight weightGoal) ++ " lbs"
    putStrLn $ "Start weight: " ++ formatDouble (startWeight weightGoal) ++ " lbs"
    -}

    t <- getCurrentTime
    weightTimeSeries <- OAuth2.evalOAuth2 tp $
        getWeightTimeSeries app fitbitApiUrl (Ending (utctDay t) Max)

    let conf = awsConfig (Local "localhost" 4569)
    dynamoDBSession <- connect conf dynamoDBService

    putWeightSamples
        tableName
        weightTimeSeries
        dynamoDBSession

bucketName :: BucketName
bucketName = BucketName "fitbit-demo"

objectKey :: ObjectKey
objectKey = ObjectKey "data.json"

doPutObject :: BucketName -> ObjectKey -> ByteString -> S3Session -> IO ()
doPutObject bucketName objectKey bytes = withAWS $ do
    void $ send $ putObject bucketName objectKey (Hashed $ toHashed bytes)

run :: Options -> IO ()
run _ = do
    let conf = awsConfig (Local "localhost" 4569)
    dynamoDBSession <- connect conf dynamoDBService
    weightSamples <- getWeightSamples tableName dynamoDBSession
    --ByteString.writeFile "data.json" (Aeson.encode weightSamples)

    conf <- getAWSConfigFromEnv
    s3Session <- connect conf s3Service
    doPutObject bucketName objectKey (Aeson.encode weightSamples) s3Session
    putStrLn "DONE"
