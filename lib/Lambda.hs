{-# LANGUAGE OverloadedStrings #-}

module Lambda
    ( clientInfoName
    , tokenPairName
    ) where

import           Lib.Params

clientInfoName :: ParameterName
clientInfoName = ParameterName "/FitbitChart/FitbitAPI/ClientInfo"

tokenPairName :: ParameterName
tokenPairName = ParameterName "/FitbitChart/FitbitAPI/TokenPair"
