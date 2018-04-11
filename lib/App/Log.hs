module App.Log
    ( logError
    , logInfo
    ) where

import           Control.Monad.IO.Class (MonadIO(..))
import           System.IO (hFlush, stdout)

logHelper :: MonadIO m => String -> String -> m ()
logHelper t s = liftIO (putStrLn ("[" ++ t ++ "] " ++ s) >> hFlush stdout)

logInfo :: MonadIO m => String -> m ()
logInfo = logHelper "info"

logError :: MonadIO m => String -> m ()
logError = logHelper "error"
