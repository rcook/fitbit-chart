{-# LANGUAGE DataKinds #-}

module FitbitDemoLib.OAuth2Types
    ( APIAction
    , APIResult
    ) where

import           Network.HTTP.Req (Scheme(..), Url)
import qualified Network.HTTP.Req.OAuth2 as OAuth2
                    ( App(..)
                    , TokenPair(..)
                    )

type APIResult a = Either String a

type APIAction a =
    Url 'Https
    -> OAuth2.App
    -> OAuth2.TokenPair
    -> IO (APIResult a, OAuth2.TokenPair)
