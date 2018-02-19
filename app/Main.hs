{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Main (main) where

{-
import           Control.Monad
import           Data.Maybe
import           Data.Text (Text)
import           Text.URI
-}
import           Control.Monad.IO.Class
import           Data.Aeson
import           Data.Aeson.Types (parseEither)
import           Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString (append, concat, putStrLn)
import qualified Data.ByteString.Base64 as Base64 (encode)
import           Data.Default.Class
import           Data.Monoid
import           Data.Text (Text)
import qualified Data.Text.Encoding as Text (encodeUtf8)
import           FitbitDemo
import           FitbitDemoApp
import           GHC.Generics
import           Network.HTTP.Req
import           Network.HTTP.Req.Url.Extra (toUrlHttps)
import           Text.URI.QQ (uri)

{-
fitbitAuthorizationUri :: Url 'Https
fitbitAuthorizationUri =
    let Just (url, _) = parseUrlHttps "https://www.fitbit.com/oauth2/authorize"
    in url
-}

data AccessTokenRequest = AccessTokenRequest ClientId AuthCode deriving Show
instance ToJSON AccessTokenRequest where
    toJSON (AccessTokenRequest (ClientId cid) (AuthCode ac)) =
        object
            [ "code" .= ac
            , "grant_type" .= ("authorization_code" :: Text)
            , "client_id" .= cid
            , "redirect_uri" .= ("http://localhost:8765/callback/" :: Text) -- TODO: Should come from Config!
            ]



data AccessTokenResponse = AccessTokenResponse AccessToken RefreshToken deriving Show

instance FromJSON AccessTokenResponse where
    parseJSON =
        withObject "AccessTokenResponse" $ \v -> AccessTokenResponse
            <$> (AccessToken <$> v .: "access_token")
            <*> (RefreshToken <$> v .: "refresh_token")


doIt :: Url 'Https -> AuthCode -> ClientId -> ClientSecret -> IO (Either String AccessTokenResponse)
doIt url authCode@(AuthCode ac) clientId@(ClientId cid) clientSecret = runReq def $ do
    let opts = header "Authorization" (ByteString.append "Basic " (encodeClientAuth clientId clientSecret))
        --tokenReq = AccessTokenRequest clientId authCode
        bbb = "code" =: ac <> "grant_type" =: ("authorization_code" :: Text) <> "client_id" =: cid
    body <- responseBody <$> req POST url (ReqBodyUrlEnc bbb) jsonResponse opts
    return $ parseEither parseJSON body

encodeClientAuth :: ClientId -> ClientSecret -> ByteString
encodeClientAuth (ClientId cId) (ClientSecret s) = Base64.encode $ ByteString.concat [Text.encodeUtf8 cId, ":", Text.encodeUtf8 s]


main :: IO ()
main = do
    {-
    Just (Config (FitbitAPI clientId clientSecret)) <- getConfig
    authCode <- getAuthCode clientId
    let Just (url, _) = toUrlHttps [uri|https://api.fitbit.com/oauth2/token|]
    result <- doIt url authCode clientId clientSecret
    print result
    putStrLn "Done"
    -}
    Just (TokenConfig accessToken refreshToken) <- getTokenConfig
    print accessToken