module FitbitChartLambda.Util
    ( withLambda
    ) where

import           AWSLambda (lambdaMain)
import           FitbitChart.App (logError, logInfo)
import           Control.Exception (SomeException, bracket_, catch)
import           Data.Aeson (Value)

withLambda :: (Value -> IO ()) -> IO ()
withLambda body =
    bracket_
        (logInfo "fitbit-chart-lambda started")
        (logInfo "fitbit-chart-lambda stopped")
        (lambdaMain body)
        `catch` (\e -> logError $ "Unhandled exception: " ++ show (e :: SomeException))
