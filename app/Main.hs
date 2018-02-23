{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Main (main) where

import           Control.Monad (forM_, void)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Trans.State.Strict
import           Data.Monoid ((<>))
import qualified Data.Text.IO as Text (getLine, putStrLn)
import           Data.Time.Clock (UTCTime(..), getCurrentTime)
import           FitbitDemoApp
import           FitbitDemoLib
import           Network.HTTP.Req
                    ( (/:)
                    , Scheme(..)
                    , Url
                    , https
                    )
import qualified Network.HTTP.Req.OAuth2 as OAuth2
                    ( App(..)
                    , ClientId(..)
                    , ClientPair(..)
                    , ClientSecret(..)
                    , PromptForCallbackURI
                    , TokenPair
                    )
import           Text.Printf (printf)
import qualified Text.URI as URI (mkURI, render)
import           Text.URI.QQ (uri)

promptForAppConfig :: AppConfigPrompt
promptForAppConfig = do
    putStrLn "No Fitbit API configuration was found."
    putStr "Enter Fitbit client ID: "
    clientId <- OAuth2.ClientId <$> Text.getLine
    putStr "Enter Fitbit client secret: "
    clientSecret <- OAuth2.ClientSecret <$> Text.getLine
    return $ AppConfig (OAuth2.ClientPair clientId clientSecret)

promptForCallbackUri :: OAuth2.PromptForCallbackURI
promptForCallbackUri authUri' = do
    putStrLn "Open following link in browser:"
    Text.putStrLn $ URI.render authUri'
    putStr "Enter callback URI: "
    URI.mkURI =<< Text.getLine

configDir :: FilePath
configDir = ".fitbit-demo"

fitbitApp :: OAuth2.App
fitbitApp =
    OAuth2.App
        [uri|https://www.fitbit.com/oauth2/authorize|]  -- authUri
        [uri|https://api.fitbit.com/oauth2/token|]      -- tokenUri

fitbitApiUrl :: Url 'Https
fitbitApiUrl = https "api.fitbit.com" /: "1"

formatDouble :: Double -> String
formatDouble = printf "%.1f"

type OAuth2App = StateT OAuth2.TokenPair IO

foo :: (OAuth2.TokenPair -> t1 -> IO (a, OAuth2.TokenPair)) -> t1 -> OAuth2App a
foo refresher action = do
    tp <- get
    (result, tp') <- liftIO $ refresher tp action
    put tp'
    return result

runOAuth2App tokenPair action = void $ flip runStateT tokenPair action

main :: IO ()
main = do
    Right (AppConfig clientPair) <- getAppConfig configDir promptForAppConfig
    Right (TokenConfig tp0) <- getTokenConfig configDir fitbitApp clientPair promptForCallbackUri

    let withRefresh' = withRefresh (writeTokenConfig configDir . TokenConfig) fitbitApp fitbitApiUrl clientPair

    runOAuth2App tp0 $ do
        Right weightGoal <- foo withRefresh' getWeightGoal
        liftIO $ do
            Text.putStrLn $ "Goal type: " <> goalType weightGoal
            putStrLn $ "Goal weight: " ++ formatDouble (goalWeight weightGoal) ++ " lbs"
            putStrLn $ "Start weight: " ++ formatDouble (startWeight weightGoal) ++ " lbs"

        t <- liftIO getCurrentTime
        let range = Ending (utctDay t) Max

        Right weightTimeSeries <- foo withRefresh' (getWeightTimeSeries range)
        forM_ (take 5 weightTimeSeries) $ \(WeightSample day value) ->
            liftIO $ putStrLn $ show day ++ ": " ++ formatDouble value ++ " lbs"

    putStrLn "DONE"
