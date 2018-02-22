{-# LANGUAGE OverloadedStrings #-}

module ConfigSpec (spec) where

import           FitbitDemoLib
import           OAuth2
import           Test.Hspec

spec :: Spec
spec =
    describe "encodeYAML" $ do
        it "yields pretty YAML" $ do
            let appConfig = AppConfig (FitbitAPI (ClientId "xyz") (ClientSecret "abc"))
            encodeYAML appConfig `shouldBe` "fitbit-api:\n  client-secret: abc\n  client-id: xyz\n"
        it "yields original value when decoded" $ do
            let appConfig = AppConfig (FitbitAPI (ClientId "xyz") (ClientSecret "abc"))
            decodeYAML (encodeYAML appConfig) `shouldBe` Just appConfig
