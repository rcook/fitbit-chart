{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import           App
import qualified Data.Aeson as Aeson (encode)
import           Data.List (sortOn)
import           Data.Monoid ((<>))
import qualified Data.Text.IO as Text (getLine, putStrLn)
import           Data.Time.Clock (UTCTime(..), getCurrentTime)
import           Lib.AWS
import           Lib.FitbitAPI
import           Lib.Storage
import           Network.AWS.Easy (connect)
import           Network.AWS.S3 (BucketName(..), ObjectKey(..))
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
import           System.IO (hFlush, stdout)
import qualified Text.URI as URI (mkURI, render)

data Options = Options

pOptions :: Parser Options
pOptions = pure Options

configDir :: FilePath
configDir = ".fitbit-demo"

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
            (helper <*> pOptions)
            (fullDesc <> progDesc "Dump Richard's Fitbit data into a JSON file stored on S3")

fetchWeightSamples :: IO [WeightSample]
fetchWeightSamples = do
    logInfo "Querying Fitbit"
    AppConfig clientPair <- exitOnFailure $ getAppConfig configDir promptForAppConfig

    let app = mkApp
                (writeTokenConfig configDir . TokenConfig)
                clientPair

    TokenConfig tp <- exitOnFailure $ getTokenConfig configDir app promptForCallbackUri

    t <- getCurrentTime
    weightTimeSeries <- OAuth2.evalOAuth2 tp $
        getWeightTimeSeries app fitbitApiUrl (Ending (utctDay t) Max)


    return $ sortOn (\(WeightSample day _) -> day) weightTimeSeries

run :: Options -> IO ()
run _ = do
    logInfo "Start"
    conf <- getAWSConfigFromEnv

    logInfo "Reading from DynamoDB"
    weightSamples <- fetchWeightSamples

    logInfo "Generate JSON file in S3"
    s3Session <- connect conf s3Service
    putBytes bucketName objectKey (Aeson.encode weightSamples) s3Session
    logInfo "End"
