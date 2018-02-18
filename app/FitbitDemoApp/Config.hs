module FitbitDemoApp.Config
    ( getConfig
    ) where

import qualified Data.Text.IO as Text (getLine)
import           FitbitDemo
import           System.Directory (createDirectoryIfMissing, doesFileExist, getHomeDirectory)
import           System.FilePath ((</>), takeDirectory)

configDir :: FilePath
configDir = ".fitbit-demo"

getConfigPath :: IO FilePath
getConfigPath = do
    homeDir <- getHomeDirectory
    return $ homeDir </> configDir </> "config.yaml"

promptForConfig :: IO Config
promptForConfig = do
    putStrLn "No Fitbit API configuration was found."
    putStr "Enter Fitbit client ID: "
    clientId <- ClientId <$> Text.getLine
    putStr "Enter Fitbit client secret: "
    clientSecret <- ClientSecret <$> Text.getLine
    return $ Config (FitbitAPI clientId clientSecret)

newConfig :: FilePath -> IO Config
newConfig p = do
    config <- promptForConfig
    createDirectoryIfMissing True (takeDirectory p)
    encodeYAMLFile p config
    return config

getConfig :: IO (Maybe Config)
getConfig = do
    configPath <- getConfigPath
    configPathExists <- doesFileExist configPath
    if configPathExists
        then decodeYAMLFile configPath
        else Just <$> newConfig configPath
