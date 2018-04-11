{-# LANGUAGE OverloadedStrings #-}

module App.FitbitConfig
    ( getClientInfo
    , getTokenPair
    , setTokenPair
    ) where

import           Control.Exception (throwIO)
import           Data.Monoid ((<>))
import           Data.Text (Text)
import qualified Data.Text as Text (splitOn)
import           Lib.AWS
import           Lib.Errors
import           Lib.Params
import qualified Network.HTTP.Req.OAuth2 as OAuth2
                    ( AccessToken(..)
                    , ClientId(..)
                    , ClientPair(..)
                    , ClientSecret(..)
                    , RefreshToken(..)
                    , TokenPair(..)
                    )

pPair :: Text -> Maybe (Text, Text)
pPair s =
    case Text.splitOn ";" s of
        s1 : s2 : [] -> return (s1, s2)
        _ -> Nothing

pHelper :: (a -> b -> c) -> (Text -> a) -> (Text -> b) -> Text -> Maybe c
pHelper c0 c1 c2 s = (\(s1, s2) -> c0 (c1 s1) (c2 s2)) <$> (pPair s)

pClientInfo :: Text -> Maybe OAuth2.ClientPair
pClientInfo = pHelper OAuth2.ClientPair OAuth2.ClientId OAuth2.ClientSecret

pTokenPair :: Text -> Maybe OAuth2.TokenPair
pTokenPair = pHelper OAuth2.TokenPair OAuth2.AccessToken OAuth2.RefreshToken

getPairHelper :: String -> ParameterName -> (Text -> Maybe a) -> SSMSession -> IO a
getPairHelper label parameterName p ssmSession = do
    s <- getSecureStringParameter parameterName ssmSession
    case p s of
        Nothing -> throwIO $ RuntimeError ("Could not parse " ++ label)
        Just result -> return result

getClientInfo :: ParameterName -> SSMSession -> IO OAuth2.ClientPair
getClientInfo clientInfoName = getPairHelper "Fitbit API info" clientInfoName pClientInfo

getTokenPair :: ParameterName -> SSMSession -> IO OAuth2.TokenPair
getTokenPair tokenPairName = getPairHelper "token pair" tokenPairName pTokenPair

setTokenPair :: ParameterName -> OAuth2.TokenPair -> SSMSession -> IO ()
setTokenPair tokenPairName (OAuth2.TokenPair (OAuth2.AccessToken at) (OAuth2.RefreshToken rt)) ssmSession =
    let s = at <> ";" <> rt
    in setSecureStringParameter tokenPairName s ssmSession
