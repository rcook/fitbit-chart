module FitbitChart.DataAccess.Types
    ( TableName(..)
    ) where

import           Data.Text (Text)

newtype TableName = TableName Text deriving Show
