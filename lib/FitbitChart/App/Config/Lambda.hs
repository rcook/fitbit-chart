{-|
Module      : FitbitChart.App.Config.Lambda
Description : Lambda application configuratoin
Copyright   : (C) Richard Cook, 2018
Licence     : MIT
Maintainer  : rcook@rcook.org
Stability   : stable
Portability : portable

Lambda application configuratoin
-}

{-# LANGUAGE OverloadedStrings #-}

module  FitbitChart.App.Config.Lambda
    ( clientInfoName
    , tokenPairName
    ) where

import           FitbitChart.SSM (ParameterName(..))

clientInfoName :: ParameterName
clientInfoName = ParameterName "/FitbitChart/FitbitAPI/ClientInfo"

tokenPairName :: ParameterName
tokenPairName = ParameterName "/FitbitChart/FitbitAPI/TokenPair"
