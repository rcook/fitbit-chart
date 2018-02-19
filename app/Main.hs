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
import           System.Directory (doesFileExist, getHomeDirectory)
import           System.FilePath ((</>))
import           Text.URI.QQ (uri)

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
        formBody = "code" =: ac <> "grant_type" =: ("authorization_code" :: Text) <> "client_id" =: cid <> "expires_in" =: ("3600" :: Text)
    body <- responseBody <$> req POST url (ReqBodyUrlEnc formBody) jsonResponse opts
    return $ parseEither parseJSON body

encodeClientAuth :: ClientId -> ClientSecret -> ByteString
encodeClientAuth (ClientId cId) (ClientSecret s) = Base64.encode $ ByteString.concat [Text.encodeUtf8 cId, ":", Text.encodeUtf8 s]

-- TODO: Move to Config
readTokenConfig :: IO TokenConfig
readTokenConfig = do
    Just (Config (FitbitAPI clientId clientSecret)) <- getConfig
    authCode <- getAuthCode clientId
    let Just (url, _) = toUrlHttps [uri|https://api.fitbit.com/oauth2/token|]
    result <- doIt url authCode clientId clientSecret
    let (AccessTokenResponse at rt) = case result of
                                        Left e -> error e
                                        Right x -> x
        tokenConfig = TokenConfig at rt
    tokenConfigPath <- getTokenConfigPath
    encodeYAMLFile tokenConfigPath tokenConfig
    return tokenConfig

-- TODO: Move to Config
configDir :: FilePath
configDir = ".fitbit-demo"

-- TODO: Move to Config
getTokenConfigPath :: IO FilePath
getTokenConfigPath = do
    homeDir <- getHomeDirectory
    return $ homeDir </> configDir </> "token.yaml"

-- TODO: Move to Config
myGetTokenConfig :: IO TokenConfig
myGetTokenConfig = do
    tokenConfigPath <- getTokenConfigPath
    tokenConfigPathExists <- doesFileExist tokenConfigPath
    if tokenConfigPathExists
        then do
            Just tokenConfig <- getTokenConfig
            return tokenConfig
        else readTokenConfig

main :: IO ()
main = do
    TokenConfig (AccessToken ac) refreshToken <- myGetTokenConfig
    let ac' = Text.encodeUtf8 ac
    result <- responseBody <$> (runReq def $
                req GET
                    (https "api.fitbit.com" /: "1" /: "user" /: "-" /: "body" /: "log" /: "weight" /: "goal.json")
                    NoReqBody
                    jsonResponse
                    (oAuth2Bearer ac' <> header "Accept-Language" "en_US"))
    print (result :: Value)
