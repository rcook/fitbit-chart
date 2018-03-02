{-# LANGUAGE DataKinds #-}

module FitbitDemoLib.OAuth2Types
    ( APIAction
    , APIResult
    , App'(..)
    , OAuth2App
    , UpdateTokenPair
    ) where

import           Control.Monad.Trans.State.Strict (StateT)
import           Network.HTTP.Req (Scheme(..), Url)
import qualified Network.HTTP.Req.OAuth2 as OAuth2 (App(..), ClientPair(..), TokenPair(..))

type OAuth2App = StateT OAuth2.TokenPair IO

type UpdateTokenPair = OAuth2.TokenPair -> IO ()

type APIResult a = Either String a

data App' = App' UpdateTokenPair OAuth2.App OAuth2.ClientPair

type APIAction a =
    Url 'Https
    -> App'
    -> OAuth2.TokenPair
    -> IO (APIResult a, OAuth2.TokenPair)
