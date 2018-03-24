{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module FitbitDemoApp.Types
    ( DynamoDBService
    , DynamoDBSession
    , TableName(..)
    , dynamoDBService
    ) where

import           Data.Text (Text)
import           Network.AWS.DynamoDB (dynamoDB)
import           Network.AWS.Easy (wrapAWSService)

newtype TableName = TableName Text deriving Show

wrapAWSService 'dynamoDB "DynamoDBService" "DynamoDBSession"
