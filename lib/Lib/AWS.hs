{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Lib.AWS
    ( DynamoDBService
    , DynamoDBSession
    , S3Service
    , S3Session
    , dynamoDBService
    , s3Service
    ) where

import           Network.AWS.DynamoDB (dynamoDB)
import           Network.AWS.Easy (wrapAWSService)
import           Network.AWS.S3 (s3)

wrapAWSService 'dynamoDB "DynamoDBService" "DynamoDBSession"
wrapAWSService 's3 "S3Service" "S3Session"
