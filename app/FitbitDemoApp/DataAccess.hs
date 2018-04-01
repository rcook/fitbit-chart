{-# LANGUAGE OverloadedStrings #-}

module FitbitDemoApp.DataAccess
    ( getWeightSamples
    , putWeightSample
    , putWeightSamples
    ) where

import           Control.Error.Util (note)
import           Control.Exception (throwIO)
import           Control.Lens ((&), (.~), (^.))
import           Control.Monad (mapM, void)
import           Control.Monad.IO.Class (liftIO)
import           Data.Foldable (for_)
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap (fromList, lookup)
import           Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty (fromList)
import           Data.List.Split (chunksOf)
import           Data.Text (Text)
import           FitbitDemoApp.Types
import           FitbitAPI
import           Network.AWS (send)
import           Network.AWS.Data (toText)
import           Network.AWS.DynamoDB
                    ( AttributeValue
                    , WriteRequest
                    , attributeValue
                    , avN
                    , avS
                    , batchWriteItem
                    , bwiRequestItems
                    , piItem
                    , prItem
                    , putItem
                    , putRequest
                    , scan
                    , srsItems
                    , wrPutRequest
                    , writeRequest
                    )
import           Network.AWS.Easy (withAWS)
import           Util.Format (formatDay)
import           Util.Parser (parseDay, parseDouble)

type Item = HashMap Text AttributeValue

getWeightSamples :: TableName -> DynamoDBSession -> IO [WeightSample]
getWeightSamples (TableName tableName) = withAWS $ do
    response <- send $ scan tableName
    case mapM deserializeWeightSample (response ^. srsItems) of
        Left e -> liftIO $ throwIO (RuntimeError e)
        Right items -> return items

putWeightSample :: TableName -> WeightSample -> DynamoDBSession -> IO ()
putWeightSample (TableName tableName) weightSample =
    withAWS (void $ send $ putItem tableName & piItem .~ serializeWeightSample weightSample)

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
                                                wrPutRequest .~ Just (putRequest & prItem .~ serializeWeightSample weightSample))
                                        weightSamples

deserializeWeightSample :: Item -> Either String WeightSample
deserializeWeightSample item = do
    dayAttr <- note "No \"date\" field" (HashMap.lookup "date" item)
    dayStr <- note "\"date\" field does not have expected type" $ dayAttr ^. avS
    day <- parseDay dayStr

    weightAttr <- note "No \"weight\" field" (HashMap.lookup "weight" item)
    weightStr <- note "\"weight\" field does not have expected type" $ weightAttr ^. avN
    weight <- parseDouble weightStr
    return $ WeightSample day weight

serializeWeightSample :: WeightSample -> Item
serializeWeightSample (WeightSample day weight) = HashMap.fromList
    [ ("date", attributeValue & avS .~ Just (formatDay day))
    , ("weight", attributeValue & avN .~ Just (toText weight))
    ]
