{-# LANGUAGE DataKinds #-}

module FitbitDemoLib.OAuth2Helper
    ( mkOAuth2Call
    ) where

import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Trans.State.Strict (get, put)
import           Network.HTTP.Req
                    ( Scheme(..)
                    , Url
                    )
import           Network.HTTP.Req.OAuth2 (OAuth2)
import           Network.HTTP.Req.OAuth2
                    ( APIAction
                    , APIResult
                    , App(..)
                    )

mkOAuth2Call ::
    (OAuth2 (APIResult a) -> OAuth2 a)
    -> App
    -> Url 'Https
    -> APIAction a
    -> OAuth2 a
mkOAuth2Call f app apiUrl action = f $ wrap (action apiUrl app)
    where
        wrap a = do
            tp <- get
            (result, tp') <- liftIO $ a tp
            put tp'
            return result
