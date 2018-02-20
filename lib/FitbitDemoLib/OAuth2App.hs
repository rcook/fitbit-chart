{-# LANGUAGE DataKinds #-}

module FitbitDemoLib.OAuth2App
    ( OAuth2App(..)
    ) where

import           Network.HTTP.Req
                    ( Scheme(..)
                    , Url
                    )
import           Text.URI (URI)

data OAuth2App = OAuth2App
    { tokenRequestUrl :: Url 'Https
    , authUri :: URI
    }
