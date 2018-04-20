{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import           Data.Monoid ((<>))
import           Data.Text (Text)
import qualified Data.Text.IO as Text (getLine, putStrLn)
import           FitbitChart.App
import           FitbitChart.SSM
import           FitbitChart.Util
import           FitbitChartParams.CommandLine
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
run (Options configPath outputPath) = do
    -- UGLY
    -- I'm not proud of this code at all
    Just (AppConfig clientPair@(OAuth2.ClientPair clientId@(OAuth2.ClientId cid) (OAuth2.ClientSecret cs))) <- decodeYAMLFile configPath
    let app = mkApp (\_ -> return ()) clientPair
    authCode <- OAuth2.getAuthCode app clientId promptForCallbackUri
    result <- OAuth2.fetchAccessToken app (OAuth2.AccessTokenRequest authCode)
    case result of
        Left e -> putStrLn e
        Right (OAuth2.AccessTokenResponse (OAuth2.TokenPair (OAuth2.AccessToken accessToken) (OAuth2.RefreshToken refreshToken))) -> do
            let ParameterName cin = clientInfoName
                ParameterName tpn = tokenPairName
            let obj = HashMap.fromList
                        [ (cin, mkPair cid cs)
                        , (tpn, mkPair accessToken refreshToken)
                        ] :: HashMap Text Text
            encodeYAMLFile outputPath obj

promptForCallbackUri :: OAuth2.PromptForCallbackUri
promptForCallbackUri authUri' = do
    putStrLn "Open following link in browser:"
    Text.putStrLn $ URI.render authUri'
    putStr "Enter callback URI including authorization code: "
    hFlush stdout
    URI.mkURI =<< Text.getLine
