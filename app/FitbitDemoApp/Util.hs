module FitbitDemoApp.Util
    ( exitOnFailure
    ) where

import           Control.Monad.IO.Class (MonadIO, liftIO)
import           System.Exit (exitFailure)

exitOnFailure :: (MonadIO m) => m (Either String a) -> m a
exitOnFailure action = do
    result <- action
    case result of
        Left e -> liftIO (putStrLn e >> exitFailure)
        Right x -> return x
