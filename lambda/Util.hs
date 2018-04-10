module Util
    ( withLambda
    ) where

import           AWSLambda (lambdaMain)
import           App.Log (logError, logInfo)
import           Control.Exception (SomeException, bracket_, catch)
import           Data.Aeson (Value)

withLambda :: (Value -> IO ()) -> IO ()
withLambda body =
    bracket_
        (logInfo "fitbit-demo-lambda started")
        (logInfo "fitbit-demo-lambda stopped")
        (lambdaMain body)
        `catch` (\e -> logError $ "Unhandled exception: " ++ show (e :: SomeException))
