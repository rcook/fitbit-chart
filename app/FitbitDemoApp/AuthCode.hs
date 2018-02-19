{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module FitbitDemoApp.AuthCode
    ( AuthCode(..)
    , getAuthCode
    ) where

import           Control.Lens ((^..), (.~), (&))
import           Data.Text (Text)
import qualified Data.Text.IO as Text (getLine, putStrLn)
import           FitbitDemo
import           Text.URI (QueryParam(..), URI, mkQueryKey, mkQueryValue, mkURI, render, unRText)
import           Text.URI.Lens (queryParam, uriQuery)
import           Text.URI.QQ (uri)

authUri :: URI
authUri = [uri|https://www.fitbit.com/oauth2/authorize|]

foo :: [(Text, Text)] -> Maybe [QueryParam]
foo qs = mapM (\(k, v) -> do
                qk <- mkQueryKey k
                qv <- mkQueryValue v
                return $ QueryParam qk qv) qs

bar :: URI -> [(Text, Text)] -> Maybe URI
bar u qs = do
    qs' <- foo qs
    return $ u & uriQuery .~ qs'

newtype AuthCode = AuthCode Text deriving Show

getAuthCode :: ClientId -> IO AuthCode
getAuthCode (ClientId clientId) = do
    let Just u = bar authUri
                    [ ("client_id", clientId)
                    , ("response_type", "code")
                    , ("scope", "weight")
                    ]
        uText = render u
    putStrLn "Open following link in browser:"
    Text.putStrLn uText

    putStr "Enter callback URI: "
    callbackUriString <- Text.getLine
    callbackUri <- mkURI callbackUriString
    codeKey <- mkQueryKey "code"
    let code = head $ callbackUri ^.. uriQuery . queryParam codeKey
    return $ AuthCode (unRText code)

