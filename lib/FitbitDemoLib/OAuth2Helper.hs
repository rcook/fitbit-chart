module FitbitDemoLib.OAuth2Helper
    ( mkOAuth2Call
    ) where

import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Trans.State.Strict (get, put)
import           Network.HTTP.Req.OAuth2 (OAuth2)
import           Network.HTTP.Req.OAuth2
                    ( APIAction
                    , APIResult
                    , App(..)
                    )

mkOAuth2Call ::
    App
    -> APIAction a
    -> OAuth2 (APIResult a)
mkOAuth2Call app action = do
    tp <- get
    (result, tp') <- liftIO $ action app tp
    put tp'
    return result
