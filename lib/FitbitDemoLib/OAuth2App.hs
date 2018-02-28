{-# LANGUAGE RankNTypes #-}

module FitbitDemoLib.OAuth2App
    ( runOAuth2App
    ) where

import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Trans.State.Strict
import           FitbitDemoLib.OAuth2Types
import qualified Network.HTTP.Req.OAuth2 as OAuth2 (TokenPair)

type OAuth2App = StateT OAuth2.TokenPair IO
type APIActionWithRefresh a = APIAction a -> OAuth2.TokenPair -> IO (APIResult a, OAuth2.TokenPair)

runOAuth2App ::
    OAuth2.TokenPair
    -> (forall a . APIActionWithRefresh a)
    -> ((forall b . APIAction b -> OAuth2App (APIResult b)) -> OAuth2App c)
    -> IO (c, OAuth2.TokenPair)
runOAuth2App tokenPair wrapWithRefresh action = runStateT (action wrap) tokenPair
    where
        wrap a = do
            tp <- get
            (result, tp') <- liftIO $ wrapWithRefresh a tp
            put tp'
            return result
