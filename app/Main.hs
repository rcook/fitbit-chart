{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}

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
import           System.IO (hFlush, stdout)
import           Text.Printf (printf)
import qualified Text.URI as URI (mkURI, render)
import           Text.URI.QQ (uri)

promptForAppConfig :: AppConfigPrompt
promptForAppConfig = do
    putStrLn "No Fitbit API configuration was found."
    putStr "Enter Fitbit client ID: "
    hFlush stdout
    clientId <- OAuth2.ClientId <$> Text.getLine
    putStr "Enter Fitbit client secret: "
    hFlush stdout
    clientSecret <- OAuth2.ClientSecret <$> Text.getLine
    return $ AppConfig (OAuth2.ClientPair clientId clientSecret)

promptForCallbackUri :: OAuth2.PromptForCallbackURI
promptForCallbackUri authUri' = do
    putStrLn "Open following link in browser:"
    Text.putStrLn $ URI.render authUri'
    putStr "Enter callback URI including authorization code: "
    hFlush stdout
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

type Blah a = OAuth2.TokenPair -> APIAction a -> IO (Either String a, OAuth2.TokenPair)

type Context a b = (OAuth2.TokenPair -> b -> IO (a, OAuth2.TokenPair)) -> b -> OAuth2App a

foo :: Context a b
foo refresher action = do
    tp <- get
    (result, tp') <- liftIO $ refresher tp action
    put tp'
    return result

runOAuth2App ::
    OAuth2.TokenPair
    -> (forall a . Blah a)
    -> ((forall b . APIAction b -> OAuth2App (Either String b)) -> OAuth2App c)
    -> IO ()
runOAuth2App tokenPair withRefresh'' action =
    let foo' :: forall a . APIAction a -> OAuth2App (Either String a)
        foo' = foo withRefresh''
    in
    void $ flip runStateT tokenPair (action foo')

main :: IO ()
main = do
    AppConfig clientPair <- exitOnFailure $ getAppConfig configDir promptForAppConfig
    TokenConfig tp0 <- exitOnFailure $ getTokenConfig configDir fitbitApp clientPair promptForCallbackUri

    runOAuth2App tp0 (withRefresh (writeTokenConfig configDir . TokenConfig) fitbitApp fitbitApiUrl clientPair) $ \fooWithRefresh -> do
        weightGoal <- exitOnFailure $ fooWithRefresh getWeightGoal
        liftIO $ do
            Text.putStrLn $ "Goal type: " <> goalType weightGoal
            putStrLn $ "Goal weight: " ++ formatDouble (goalWeight weightGoal) ++ " lbs"
            putStrLn $ "Start weight: " ++ formatDouble (startWeight weightGoal) ++ " lbs"

        t <- liftIO getCurrentTime
        let range = Ending (utctDay t) Max

        weightTimeSeries <- exitOnFailure $ fooWithRefresh (getWeightTimeSeries range)
        forM_ (take 5 weightTimeSeries) $ \(WeightSample day value) ->
            liftIO $ putStrLn $ show day ++ ": " ++ formatDouble value ++ " lbs"

    putStrLn "DONE"
