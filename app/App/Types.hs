{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module App.Types
    ( DynamoDBService
    , DynamoDBSession
    , RuntimeError(..)
    , S3Service
    , S3Session
    , TableName(..)
    , dynamoDBService
    , s3Service
    ) where

import           Control.Exception (Exception)
import           Data.Text (Text)
import           Network.AWS.DynamoDB (dynamoDB)
import           Network.AWS.Easy (wrapAWSService)
import           Network.AWS.S3 (s3)

newtype TableName = TableName Text deriving Show

data RuntimeError = RuntimeError String deriving Show
instance Exception RuntimeError

wrapAWSService 'dynamoDB "DynamoDBService" "DynamoDBSession"
wrapAWSService 's3 "S3Service" "S3Session"
