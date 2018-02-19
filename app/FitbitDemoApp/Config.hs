module FitbitDemoApp.Config
    ( getConfig -- TODO: Rename to getAppConfig
    , getTokenConfig
    ) where

import qualified Data.Text.IO as Text (getLine)
import           FitbitDemoLib
import           System.Directory (createDirectoryIfMissing, doesFileExist, getHomeDirectory)
import           System.FilePath ((</>), takeDirectory)

configDir :: FilePath
configDir = ".fitbit-demo"

getConfigPath :: IO FilePath
getConfigPath = do
    homeDir <- getHomeDirectory
    return $ homeDir </> configDir </> "config.yaml"

getTokenConfigPath :: IO FilePath
getTokenConfigPath = do
    homeDir <- getHomeDirectory
    return $ homeDir </> configDir </> "token.yaml"

promptForConfig :: IO AppConfig
promptForConfig = do
    putStrLn "No Fitbit API configuration was found."
    putStr "Enter Fitbit client ID: "
    clientId <- ClientId <$> Text.getLine
    putStr "Enter Fitbit client secret: "
    clientSecret <- ClientSecret <$> Text.getLine
    return $ AppConfig (FitbitAPI clientId clientSecret)

newConfig :: FilePath -> IO AppConfig
newConfig p = do
    config <- promptForConfig
    createDirectoryIfMissing True (takeDirectory p)
    encodeYAMLFile p config
    return config

getConfig :: IO (Maybe AppConfig)
getConfig = do
    configPath <- getConfigPath
    configPathExists <- doesFileExist configPath
    if configPathExists
        then decodeYAMLFile configPath
        else Just <$> newConfig configPath

getTokenConfig :: IO (Maybe TokenConfig)
getTokenConfig = do
    tokenConfigPath <- getTokenConfigPath
    -- TODO: What to do if it doesn't exist yet? Start auth code workflow etc.
    decodeYAMLFile tokenConfigPath
