module App.CommandLine
    ( Options(..)
    , optionsParser
    ) where

import           Control.Applicative ((<|>))
import           Data.Monoid ((<>))
import           Options.Applicative
                    ( Parser
                    , flag'
                    , help
                    , long
                    , metavar
                    , short
                    , strOption
                    )

data Options =
    ConfigDir FilePath
    | SSMProperties

configDirParser :: Parser Options
configDirParser = ConfigDir <$> strOption
    (long "conf"
    <> short 'c'
--    <> value "~/.fitbit-demo"
    <> metavar "CONF"
    <> help "Path to configuration directory")

ssmPropertiesParser :: Parser Options
ssmPropertiesParser = flag'
    SSMProperties
    (long "ssm"
    <> short 's'
    <> help "Use SSM parameters")

optionsParser :: Parser Options
optionsParser = configDirParser <|> ssmPropertiesParser
