module FitbitChartParams.CommandLine
    ( Mode(..)
    , Options(..)
    , optionsParser
    ) where

import           Control.Applicative ((<|>))
import           Data.Monoid ((<>))
import           Options.Applicative
                    ( Parser
                    , flag
                    , flag'
                    , help
                    , long
                    , metavar
                    , strArgument
                    )

data Mode = Simulate | NoSimulate

data Options = Options FilePath Mode

optionsParser :: Parser Options
optionsParser = Options
    <$> strArgument (metavar "CONFPATH")
    <*> pMode

pMode :: Parser Mode
pMode =
    flag Simulate NoSimulate (long "no-simulate" <> help "Do not simulate parameter generation")
    <|> flag' Simulate (long "simulate" <> help "Simulate parameter generation")
