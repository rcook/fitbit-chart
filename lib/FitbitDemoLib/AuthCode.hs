{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module FitbitDemoLib.AuthCode
    ( AuthCode(..)
    , PromptForCallbackURI
    , getAuthCode
    ) where

import           Control.Lens ((^..), (.~), (&))
import           Data.Text (Text)
import           FitbitDemoLib.Types
import           Text.URI (QueryParam(..), URI, mkQueryKey, mkQueryValue, unRText)
import           Text.URI.Lens (queryParam, uriQuery)
import           Text.URI.QQ (uri)

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

getAuthCode :: ClientId -> PromptForCallbackURI -> IO AuthCode
getAuthCode (ClientId clientId) prompt = do
    let Just authUriWithOpts = buildAuthUriWithOpts [uri|https://www.fitbit.com/oauth2/authorize|]
                                    [ ("client_id", clientId)
                                    , ("response_type", "code")
                                    , ("scope", "weight")
                                    ]
    callbackUri <- prompt authUriWithOpts
    codeKey <- mkQueryKey "code"
    let code = head $ callbackUri ^.. uriQuery . queryParam codeKey
    return $ AuthCode (unRText code)
