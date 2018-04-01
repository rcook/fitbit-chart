module Lib.Util.Misc
    ( fromJustIO
    , fromRightIO
    ) where

import           Control.Exception
                    ( Exception
                    , throwIO
                    )
import           Control.Monad.IO.Class (MonadIO(..))

fromJustIO :: (MonadIO m, Exception e) => e -> Maybe a -> m a
fromJustIO _ (Just a) = pure a
fromJustIO e _ = liftIO $ throwIO e

fromRightIO :: (MonadIO m, Exception e) => (b -> e) -> Either b a -> m a
fromRightIO _ (Right a) = pure a
fromRightIO f (Left e) = liftIO $ throwIO (f e)
