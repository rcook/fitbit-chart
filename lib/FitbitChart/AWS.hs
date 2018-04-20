{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module FitbitChart.AWS
    ( DynamoDBService
    , DynamoDBSession
    , S3Service
    , S3Session
    , SSMService
    , SSMSession
    , dynamoDBService
    , s3Service
    , ssmService
    ) where

import           Network.AWS.DynamoDB (dynamoDB)
import           Network.AWS.Easy (wrapAWSService)
import           Network.AWS.S3 (s3)
import           Network.AWS.SSM (ssm)

wrapAWSService 'dynamoDB "DynamoDBService" "DynamoDBSession"
wrapAWSService 's3 "S3Service" "S3Session"
wrapAWSService 'ssm "SSMService" "SSMSession"
