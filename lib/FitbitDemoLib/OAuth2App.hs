module FitbitDemoLib.OAuth2App
    ( OAuth2App(..)
    ) where

import           Text.URI (URI)

data OAuth2App = OAuth2App
    { tokenRequestURI :: URI
    , authURI :: URI
    }
