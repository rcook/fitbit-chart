{-# LANGUAGE OverloadedStrings #-}

module Lambda
    ( clientInfoName
    , tokenPairName
    ) where

import           FitbitChart.SSM

clientInfoName :: ParameterName
clientInfoName = ParameterName "/FitbitChart/FitbitAPI/ClientInfo"

tokenPairName :: ParameterName
tokenPairName = ParameterName "/FitbitChart/FitbitAPI/TokenPair"
