{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module FitbitDemoLib.AuthCode
    ( AuthCode(..)
    , getAuthCode
    ) where

import           Control.Lens ((^..), (.~), (&))
import           Data.Text (Text)
import           FitbitDemoLib.Types
import           Text.URI (QueryParam(..), URI, mkQueryKey, mkQueryValue, unRText)
import           Text.URI.Lens (queryParam, uriQuery)
import           Text.URI.QQ (uri)

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

getAuthCode :: ClientId -> (URI -> IO URI) -> IO AuthCode
getAuthCode (ClientId clientId) getCallbackUriAction = do
    let Just authUriWithOpts = buildAuthUriWithOpts [uri|https://www.fitbit.com/oauth2/authorize|]
                                    [ ("client_id", clientId)
                                    , ("response_type", "code")
                                    , ("scope", "weight")
                                    ]
    callbackUri <- getCallbackUriAction authUriWithOpts
    codeKey <- mkQueryKey "code"
    let code = head $ callbackUri ^.. uriQuery . queryParam codeKey
    return $ AuthCode (unRText code)
