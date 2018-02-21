{-# LANGUAGE OverloadedStrings #-}

module FitbitDemoLib.HttpUtil
    ( hasResponseStatus
    ) where

import qualified Network.HTTP.Client as HTTP (HttpException(..), HttpExceptionContent(..), responseStatus)
import           Network.HTTP.Req (HttpException(..))
import           Network.HTTP.Types (Status)

hasResponseStatus :: HttpException -> Status -> Bool
hasResponseStatus
    (VanillaHttpException (HTTP.HttpExceptionRequest _ (HTTP.StatusCodeException response _))) status =
    HTTP.responseStatus response == status
hasResponseStatus _ _ = False
