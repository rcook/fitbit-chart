{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module FitbitDemoApp.Types
    ( DynamoDBService
    , DynamoDBSession
    , RuntimeError(..)
    , TableName(..)
    , dynamoDBService
    ) where

import           Control.Exception (Exception)
import           Data.Text (Text)
import           Network.AWS.DynamoDB (dynamoDB)
import           Network.AWS.Easy (wrapAWSService)

newtype TableName = TableName Text deriving Show

data RuntimeError = RuntimeError String deriving Show
instance Exception RuntimeError

wrapAWSService 'dynamoDB "DynamoDBService" "DynamoDBSession"
