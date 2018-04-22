{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Data.Aeson as Aeson (encode)
import           Data.List (sortOn)
import           Data.Time.Clock (UTCTime(..), getCurrentTime)
import           FitbitChart.AWS
import           FitbitChart.App
import           FitbitChart.Fitbit
import           FitbitChart.S3
import           FitbitChartLambda.Util
import           Network.AWS.Easy (AWSConfig, connect)
import           Network.AWS.S3 (BucketName(..), ObjectKey(..))
import qualified Network.HTTP.Req.OAuth2 as OAuth2
                    ( App
                    , UpdateTokenPair
                    , evalOAuth2
                    )

bucketName :: BucketName
bucketName = BucketName "fitbit-chart"

objectKey :: ObjectKey
objectKey = ObjectKey "data.json"

main :: IO ()
main = withLambda $ \_ -> do
    logInfo "Start"
    conf <- getAWSConfigFromEnv

    logInfo "Reading data from Fitbit API"
    weightSamples <- fetchWeightSamples conf
    logInfo "Data read from Fitbit API"

    logInfo "Generating JSON file in S3"
    s3Session <- connect conf s3Service
    putBytes bucketName objectKey (Aeson.encode weightSamples) s3Session
    logInfo "JSON file generated in S3"
    logInfo "End"

fetchWeightSamples :: AWSConfig -> IO [WeightSample]
fetchWeightSamples conf = do
    (AppConfig clientPair, updateTokenPair, getTokenConfigAction) <- getConfig conf
    let app = mkApp updateTokenPair clientPair
    TokenConfig tp <- getTokenConfigAction app

    t <- getCurrentTime
    weightTimeSeries <- OAuth2.evalOAuth2 tp $
        getWeightTimeSeries app fitbitApiUrl (Ending (utctDay t) Max)

    return $ sortOn (\(WeightSample day _) -> day) weightTimeSeries

getConfig :: AWSConfig -> IO (AppConfig, OAuth2.UpdateTokenPair, OAuth2.App -> IO TokenConfig)
getConfig conf = do
    ssmSession <- connect conf ssmService
    cp <- getClientInfo clientInfoName ssmSession
    return
        ( AppConfig cp
        , \tp -> setTokenPair tokenPairName tp ssmSession
        , \_ -> TokenConfig <$> getTokenPair tokenPairName ssmSession
        )
