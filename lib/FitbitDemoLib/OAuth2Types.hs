{-# LANGUAGE DataKinds #-}

module FitbitDemoLib.OAuth2Types
    ( APIAction
    , APIResult
    , UpdateTokenPair
    ) where

import           Network.HTTP.Req (Scheme(..), Url)
import qualified Network.HTTP.Req.OAuth2 as OAuth2 (App(..), ClientPair(..), TokenPair(..))

type UpdateTokenPair = OAuth2.TokenPair -> IO ()

type APIResult a = Either String a

type APIAction a =
    Url 'Https
    -> UpdateTokenPair
    -> OAuth2.App
    -> OAuth2.ClientPair
    -> OAuth2.TokenPair
    -> IO (APIResult a, OAuth2.TokenPair)
