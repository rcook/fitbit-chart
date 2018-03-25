{-# LANGUAGE OverloadedStrings #-}

module FitbitDemoApp.DataAccess
    ( putWeightSample
    , putWeightSamples
    ) where

import           Control.Monad (void)
import           Data.Foldable (for_)
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import           Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import           Data.List.Split (chunksOf)
import           Data.Text (Text)
import qualified Data.Text as Text (pack)
import           Data.Time.Calendar (Day, toGregorian)
import           FitbitDemoApp.Types
import           FitbitDemoLib
import           Network.AWS.DynamoDB
import           Network.AWS.Easy
import           Text.Printf (printf)

putWeightSample :: TableName -> WeightSample -> DynamoDBSession -> IO ()
putWeightSample (TableName tableName) (WeightSample day weight) = withAWS $ do
    void $ send $ putItem tableName & piItem .~ item
    where
        item = HashMap.fromList
            [ ("date", attributeValue & avS .~ Just (dayToText day))
            , ("weight", attributeValue & avN .~ Just (toText weight))
            ]

putWeightSamples :: TableName -> [WeightSample] -> DynamoDBSession -> IO ()
putWeightSamples (TableName tableName) ws session = for_ (chunksOf 25 ws) go
    where
        go :: [WeightSample] -> IO ()
        go weightSamples = (flip withAWS) session $ do
            void $ send (batchWriteItem & bwiRequestItems .~ requestItems)
            where
                requestItems :: HashMap Text (NonEmpty WriteRequest)
                requestItems = HashMap.fromList [ (tableName, writeRequests) ]
                writeRequests = NonEmpty.fromList $
                                    map
                                        (\weightSample ->
                                            writeRequest &
                                                wrPutRequest .~ Just (putRequest & prItem .~ item weightSample))
                                        weightSamples
                item (WeightSample day weight) = HashMap.fromList
                    [ ("date", attributeValue & avS .~ Just (dayToText day))
                    , ("weight", attributeValue & avN .~ Just (toText weight))
                    ]

dayToText :: Day -> Text
dayToText d =
    let (year, month, day) = toGregorian d
    in Text.pack $ printf "%04d-%02d-%02d" year month day
