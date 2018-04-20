module CommandLine
    ( Options(..)
    , optionsParser
    ) where

import           Options.Applicative
                    ( Parser
                    , metavar
                    , strArgument
                    )

data Options = Options String String

optionsParser :: Parser Options
optionsParser = Options
    <$> strArgument (metavar "CONFPATH")
    <*> strArgument (metavar "OUTPUTPATH")
