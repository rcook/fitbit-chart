{-|
Module      : FitbitChart.DynamoDB
Description : DynamoDB helpers
Copyright   : (C) Richard Cook, 2018
Licence     : MIT
Maintainer  : rcook@rcook.org
Stability   : stable
Portability : portable

DynamoDB helpers
-}

module FitbitChart.DynamoDB
    ( TableName(..)
    ) where

import           Data.Text (Text)

newtype TableName = TableName Text deriving Show
