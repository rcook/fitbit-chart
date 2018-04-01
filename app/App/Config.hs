{-# LANGUAGE OverloadedStrings #-}

module App.Config
    ( getAWSConfigFromEnv
    , getEnvRegion
    , getAWSConfig
    ) where

import           Control.Exception (Exception, throwIO)
import           Control.Lens ((&), (.~))
import           Control.Monad.IO.Class (MonadIO(..))
import           Data.String (IsString)
import qualified Data.Text as Text (pack)
import           Network.AWS (Region)
import           Network.AWS.Auth (AccessKey, Credentials(..), SecretKey)
import           Network.AWS.Data (fromText)
import           Network.AWS.Easy (AWSConfig, Endpoint(..), awsConfig, awscCredentials)
import           System.Environment (getEnv)

data RuntimeError = RuntimeError String deriving Show
instance Exception RuntimeError

awsAccessKeyIdName :: IsString s => s
awsAccessKeyIdName = "AWS_ACCESS_KEY_ID"

awsSecretAccessKeyName :: IsString s => s
awsSecretAccessKeyName = "AWS_SECRET_ACCESS_KEY"

--awsSessionTokenName :: IsString s => s
--awsSessionTokenName = "AWS_SESSION_TOKEN"

awsRegionName :: IsString s => s
awsRegionName = "AWS_REGION"

fromRightIO :: (MonadIO m, Exception e) => (b -> e) -> Either b a -> m a
fromRightIO _ (Right a) = pure a
fromRightIO f (Left e) = liftIO $ throwIO (f e)

getEnvRegion :: String -> IO Region
getEnvRegion regionName = do
    regionStr <- getEnv regionName
    fromRightIO
        (\e -> RuntimeError ("Could not parse AWS region " ++ regionStr ++ ": " ++ e))
        (fromText (Text.pack regionStr))

getAWSConfigFromEnv :: IO AWSConfig
getAWSConfigFromEnv = do
    region <- getEnvRegion awsRegionName
    return $ awsConfig (AWSRegion region)
                & awscCredentials .~ FromEnv
                                        awsAccessKeyIdName
                                        awsSecretAccessKeyName
                                        Nothing --(Just awsSessionTokenName)
                                        (Just awsRegionName)

getAWSConfig :: Region -> AccessKey -> SecretKey -> AWSConfig
getAWSConfig region ak sk = awsConfig (AWSRegion region)
                                & awscCredentials .~ FromKeys ak sk
