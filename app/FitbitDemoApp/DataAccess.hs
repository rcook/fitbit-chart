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
import           Data.Maybe (fromJust)
import           Data.Text (Text)
import qualified Data.Text as Text (null, pack)
import qualified Data.Text.Read as Text (decimal, double)
import           Data.Time.Calendar (Day, toGregorian)
import           FitbitDemoApp.Types
import           FitbitDemoLib
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
import           Text.Printf (printf)

type Item = HashMap Text AttributeValue

getWeightSamples :: TableName -> DynamoDBSession -> IO [Double]
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

parseInt' :: Text -> Maybe Int
parseInt' s = case Text.decimal s of
    Left _ -> Nothing
    Right (result, s') -> if Text.null s' then Just result else Nothing

parseDouble' :: Text -> Maybe Double
parseDouble' s = case Text.double s of
    Left _ -> Nothing
    Right (result, s') -> if Text.null s' then Just result else Nothing

dayToText :: Day -> Text
dayToText d =
    let (year, month, day) = toGregorian d
    in Text.pack $ printf "%04d-%02d-%02d" year month day

deserializeWeightSample :: Item -> Either String Double
deserializeWeightSample item = do
    weightAttr <- note "No \"weight\" field" (HashMap.lookup "weight" item)
    weightStr <- note "\"weight\" field not expected type" $ weightAttr ^. avN
    parseDouble weightStr

serializeWeightSample :: WeightSample -> Item
serializeWeightSample (WeightSample day weight) = HashMap.fromList
    [ ("date", attributeValue & avS .~ Just (dayToText day))
    , ("weight", attributeValue & avN .~ Just (toText weight))
    ]
