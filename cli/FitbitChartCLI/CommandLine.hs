module FitbitChartCLI.CommandLine
    ( Options(..)
    , optionsParser
    ) where

import           Data.Monoid ((<>))
import           Options.Applicative
                    ( Parser
                    , help
                    , long
                    , metavar
                    , short
                    , strOption
                    , value
                    )

data Options = Options FilePath

optionsParser :: Parser Options
optionsParser = Options <$> strOption
    (long "conf"
    <> short 'c'
    <> value "~/.fitbit-chart"
    <> metavar "CONF"
    <> help "Path to configuration directory")
