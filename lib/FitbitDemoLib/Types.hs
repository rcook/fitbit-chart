module FitbitDemoLib.Types
    ( AccessToken(..)
    , ClientId(..)
    , ClientSecret(..)
    , RefreshToken(..)
    ) where

import           Data.Text (Text)

newtype ClientId = ClientId Text deriving Show

newtype ClientSecret = ClientSecret Text deriving Show

newtype AccessToken = AccessToken Text deriving Show

newtype RefreshToken = RefreshToken Text deriving Show
