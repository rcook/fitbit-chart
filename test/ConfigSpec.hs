{-# LANGUAGE OverloadedStrings #-}

module ConfigSpec (spec) where

--import           FitbitDemoLib
--import qualified Network.HTTP.Req.OAuth2 as OAuth2 (ClientId(..), ClientSecret(..))
import           Test.Hspec (Spec)

{-
spec :: Spec
spec =
    describe "encodeYAML" $ do
        it "yields pretty YAML" $ do
            let appConfig = AppConfig (FitbitAPI (OAuth2.ClientId "xyz") (OAuth2.ClientSecret "abc"))
            encodeYAML appConfig `shouldBe` "fitbit-api:\n  client-secret: abc\n  client-id: xyz\n"
        it "yields original value when decoded" $ do
            let appConfig = AppConfig (FitbitAPI (OAuth2.ClientId "xyz") (OAuth2.ClientSecret "abc"))
            decodeYAML (encodeYAML appConfig) `shouldBe` Just appConfig
-}
spec :: Spec
spec = return ()
