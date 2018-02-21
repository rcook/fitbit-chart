{-# LANGUAGE OverloadedStrings #-}

module OAuth2.AuthCode
    ( AuthCode(..)
    , PromptForCallbackURI
    , getAuthCode
    ) where

import           Control.Lens ((^..), (.~), (&))
import           Data.Text (Text)
import           OAuth2.App
import           OAuth2.Types
import           Text.URI (QueryParam(..), URI, mkQueryKey, mkQueryValue, unRText)
import           Text.URI.Lens (queryParam, uriQuery)

type PromptForCallbackURI = URI -> IO URI

newtype AuthCode = AuthCode Text deriving Show

convertParams :: [(Text, Text)] -> Maybe [QueryParam]
convertParams qs = mapM (\(k, v) -> do
                qk <- mkQueryKey k
                qv <- mkQueryValue v
                return $ QueryParam qk qv) qs

buildAuthUriWithOpts :: URI -> [(Text, Text)] -> Maybe URI
buildAuthUriWithOpts u qs = do
    qs' <- convertParams qs
    return $ u & uriQuery .~ qs'

-- | Gets OAuth2 authorization code
--
-- Implements standard OAuth2 authorization workflow for web server apps
-- as described <https://aaronparecki.com/oauth-2-simplified/#web-server-apps here>.
--
-- We don't bother with @redirect_uri@ or @state@ since they do not seem
-- to be required.
getAuthCode :: App -> ClientId -> PromptForCallbackURI -> IO AuthCode
getAuthCode oauth2 (ClientId clientId) prompt = do
    let Just authUriWithOpts = buildAuthUriWithOpts (authUri oauth2)
                                    [ ("client_id", clientId)
                                    , ("response_type", "code")
                                    , ("scope", "weight")
                                    ]
    callbackUri <- prompt authUriWithOpts
    codeKey <- mkQueryKey "code"
    let code = head $ callbackUri ^.. uriQuery . queryParam codeKey
    return $ AuthCode (unRText code)