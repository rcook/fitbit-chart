{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Main (main) where

import           Control.Exception (catch, throwIO)
import           Data.Aeson ((.:), (.=), FromJSON(..), ToJSON(..), Value, object, withObject)
import           Data.Aeson.Types (parseEither)
import           Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString (append, concat)
import qualified Data.ByteString.Base64 as Base64 (encode)
import           Data.Default.Class (def)
import           Data.Monoid ((<>))
import           Data.Text (Text)
import qualified Data.Text.Encoding as Text (encodeUtf8)
import           FitbitDemoApp
import           FitbitDemoLib
import           Network.HTTP.Client (HttpException(..), HttpExceptionContent(..), responseStatus)
import           Network.HTTP.Types (unauthorized401)
import           Network.HTTP.Req
                    ( (/:)
                    , (=:)
                    , GET(..)
                    , HttpException(..)
                    , NoReqBody(..)
                    , POST(..)
                    , ReqBodyUrlEnc(..)
                    , Scheme(..)
                    , Url
                    , header
                    , https
                    , jsonResponse
                    , oAuth2Bearer
                    , req
                    , responseBody
                    , runReq
                    )
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
doIt url (AuthCode ac) clientId@(ClientId cid) clientSecret = runReq def $ do
    let opts = header "Authorization" (ByteString.append "Basic " (encodeClientAuth clientId clientSecret))
        formBody = "code" =: ac <> "grant_type" =: ("authorization_code" :: Text) <> "client_id" =: cid <> "expires_in" =: ("3600" :: Text)
    body <- responseBody <$> req POST url (ReqBodyUrlEnc formBody) jsonResponse opts
    return $ parseEither parseJSON body

encodeClientAuth :: ClientId -> ClientSecret -> ByteString
encodeClientAuth (ClientId cId) (ClientSecret s) = Base64.encode $ ByteString.concat [Text.encodeUtf8 cId, ":", Text.encodeUtf8 s]

-- TODO: Move to Config
readTokenConfig :: AppConfig -> IO TokenConfig
readTokenConfig (AppConfig (FitbitAPI clientId clientSecret)) = do
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
myGetTokenConfig :: AppConfig -> IO TokenConfig
myGetTokenConfig config = do
    tokenConfigPath <- getTokenConfigPath
    tokenConfigPathExists <- doesFileExist tokenConfigPath
    if tokenConfigPathExists
        then do
            Just tokenConfig <- getTokenConfig
            return tokenConfig
        else readTokenConfig config

data RefreshTokenResponse = RefreshTokenResponse AccessToken RefreshToken deriving Show

instance FromJSON RefreshTokenResponse where
    parseJSON =
        withObject "RefreshTokenResponse" $ \v -> RefreshTokenResponse
            <$> (AccessToken <$> v .: "access_token")
            <*> (RefreshToken <$> v .: "refresh_token")

refreshAndInvoke :: ClientId -> ClientSecret -> TokenConfig -> IO a -> IO a
refreshAndInvoke clientId clientSecret (TokenConfig accessToken (RefreshToken rt)) action = do
    let url = https "api.fitbit.com" /: "oauth2" /: "token"
    let opts = header "Authorization" (ByteString.append "Basic " (encodeClientAuth clientId clientSecret))
        formBody = "grant_type" =: ("refresh_token" :: Text) <> "refresh_token" =: rt <> "expires_in" =: ("3600" :: Text)
    body <- runReq def $ responseBody <$> req POST url (ReqBodyUrlEnc formBody) jsonResponse opts
    putStrLn "REFRESH AND REVOKE"
    let RefreshTokenResponse at rt' = case parseEither parseJSON body of
                                        Left e -> error $ "BAIL: " ++ e
                                        Right x -> x
        newTokenConfig = TokenConfig at rt'
        AccessToken at0 = accessToken
        AccessToken at1 = at
    putStrLn $ "accessToken stayed the same: " ++ show (at0 == at1)
    tokenConfigPath <- getTokenConfigPath
    encodeYAMLFile tokenConfigPath newTokenConfig
    action

withRefresh :: ClientId -> ClientSecret -> TokenConfig -> IO a -> IO a
withRefresh clientId clientSecret tokenConfig action = do
    result <- catch action $ \e ->
                case e of
                    VanillaHttpException (HttpExceptionRequest _ (StatusCodeException response _)) ->
                        if responseStatus response == unauthorized401
                            then refreshAndInvoke clientId clientSecret tokenConfig action
                            else throwIO e
                    _ -> throwIO e
    return result

getWeightGoal :: AccessToken -> IO Value
getWeightGoal (AccessToken at) = do
    let at' = Text.encodeUtf8 at
    responseBody <$> (runReq def $
        req GET
            (https "api.fitbit.com" /: "1" /: "user" /: "-" /: "body" /: "log" /: "weight" /: "goal.json")
            NoReqBody
            jsonResponse
            (oAuth2Bearer at' <> header "Accept-Language" "en_US"))

main :: IO ()
main = do
    Just config@(AppConfig (FitbitAPI clientId clientSecret)) <- getConfig
    tokenConfig@(TokenConfig accessToken _) <- myGetTokenConfig config
    weightGoal <- withRefresh clientId clientSecret tokenConfig $ getWeightGoal accessToken
    print weightGoal

    putStrLn "DONE"
