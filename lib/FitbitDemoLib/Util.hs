{-# LANGUAGE OverloadedStrings #-}

module FitbitDemoLib.Util
    ( parseDouble
    , parseInt
    ) where

import           Data.Text (Text)
import qualified Data.Text as Text (unpack)
import qualified Data.Text.Read as Text (decimal, double)

parseInt :: Text -> Either String Int
parseInt s =
    case Text.decimal s of
        Right (result, "") -> return result
        _ -> Left $ "Could not parse int from " ++ Text.unpack s

parseDouble :: Text -> Either String Double
parseDouble s =
    case Text.double s of
        Right (result, "") -> return result
        _ -> Left $ "Could not parse double from " ++ Text.unpack s
