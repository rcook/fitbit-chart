{-|
Module      : OAuth2
Description : Basic OAuth2 support for Req
Copyright   : (C) Richard Cook, 2018
Licence     : MIT
Maintainer  : rcook@rcook.org
Stability   : stable
Portability : portable

This package provides basic support of OAuth2 authentication for <https://hackage.haskell.org/package/req Req>.
-}

module OAuth2
    ( module OAuth2.AccessToken
    , module OAuth2.App
    , module OAuth2.AuthCode
    , module OAuth2.RefreshToken
    , module OAuth2.Types
    , module OAuth2.Util
    ) where

import OAuth2.AccessToken
import OAuth2.App
import OAuth2.AuthCode
import OAuth2.RefreshToken
import OAuth2.Types
import OAuth2.Util
