{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Main (main) where

import           Control.Monad (forM_)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Trans.State.Strict (StateT, evalStateT, get, put)
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

wrap :: MonadIO m =>
    (t -> IO (a, t))
    -> StateT t m a
wrap action = do
    tp <- get
    (result, tp') <- liftIO $ action tp
    put tp'
    return result

mkCallFitbitApi :: MonadIO m =>
    c
    -> (Url 'Https
    -> (OAuth2.TokenPair -> IO ())
    -> OAuth2.App
    -> c
    -> t
    -> IO (Either String a, t))
    -> StateT t m a
mkCallFitbitApi clientPair action =
    exitOnFailure $ wrap (action fitbitApiUrl updateTokenPair fitbitApp clientPair)
    where
        updateTokenPair = writeTokenConfig configDir . TokenConfig

main :: IO ()
main = do
    AppConfig clientPair <- exitOnFailure $ getAppConfig configDir promptForAppConfig
    TokenConfig tp0 <- exitOnFailure $ getTokenConfig configDir fitbitApp clientPair promptForCallbackUri

    let callFitbitApi = mkCallFitbitApi clientPair

    t <- getCurrentTime
    (weightGoal, weightTimeSeries) <- (flip evalStateT) tp0 $ do
        weightGoal' <- callFitbitApi getWeightGoal
        weightTimeSeries' <- callFitbitApi $ getWeightTimeSeries (Ending (utctDay t) Max)
        return (weightGoal', weightTimeSeries')

    Text.putStrLn $ "Goal type: " <> goalType weightGoal
    putStrLn $ "Goal weight: " ++ formatDouble (goalWeight weightGoal) ++ " lbs"
    putStrLn $ "Start weight: " ++ formatDouble (startWeight weightGoal) ++ " lbs"
    forM_ (take 5 weightTimeSeries) $ \(WeightSample day value) ->
        liftIO $ putStrLn $ show day ++ ": " ++ formatDouble value ++ " lbs"
