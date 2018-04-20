{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import           App.Util
import           CommandLine
import           Data.Monoid ((<>))
import qualified Data.Text as Text (pack)
import qualified Data.Text.IO as Text (getLine, putStrLn)

import qualified Network.HTTP.Req.OAuth2 as OAuth2
                    ( AccessToken(..)
                    , AccessTokenRequest(..)
                    , AccessTokenResponse(..)
                    , ClientId(..)
                    , ClientPair(..)
                    , ClientSecret(..)
                    , PromptForCallbackUri
                    , RefreshToken(..)
                    , TokenPair(..)
                    , fetchAccessToken
                    , getAuthCode
                    )
import           Options.Applicative
                    ( execParser
                    , fullDesc
                    , helper
                    , info
                    , progDesc
                    )
import           System.IO (hFlush, stdout)
import qualified Text.URI as URI (mkURI, render)

main :: IO ()
main = parseOptions >>= run
    where
        parseOptions = execParser $ info
            (helper <*> optionsParser)
            (fullDesc <> progDesc "Get initial token pair")

run :: Options -> IO ()
run (Options cId cSecret) = do
    let clientId = OAuth2.ClientId (Text.pack cId)
        clientSecret = OAuth2.ClientSecret (Text.pack cSecret)
        clientPair = OAuth2.ClientPair clientId clientSecret
        app = mkApp (\_ -> return ()) clientPair
    authCode <- OAuth2.getAuthCode app clientId promptForCallbackUri
    result <- OAuth2.fetchAccessToken app (OAuth2.AccessTokenRequest authCode)
    case result of
        Left e -> putStrLn e
        Right (OAuth2.AccessTokenResponse (OAuth2.TokenPair (OAuth2.AccessToken accessToken) (OAuth2.RefreshToken refreshToken))) -> do
            Text.putStrLn $ "accessToken=" <> accessToken
            Text.putStrLn $ "refreshToken=" <> refreshToken

promptForCallbackUri :: OAuth2.PromptForCallbackUri
promptForCallbackUri authUri' = do
    putStrLn "Open following link in browser:"
    Text.putStrLn $ URI.render authUri'
    putStr "Enter callback URI including authorization code: "
    hFlush stdout
    URI.mkURI =<< Text.getLine
