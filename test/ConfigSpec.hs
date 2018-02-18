{-# LANGUAGE OverloadedStrings #-}

module ConfigSpec (spec) where

import           FitbitDemo
import           Test.Hspec

spec :: Spec
spec =
    describe "encodeYAML" $ do
        it "yields pretty YAML" $ do
            let config = Config (FitbitAPI (ClientId "xyz") (ClientSecret "abc"))
            encodeYAML config `shouldBe` "fitbit-api:\n  client-secret: abc\n  client-id: xyz\n"
        it "yields original value when decoded" $ do
            let config = Config (FitbitAPI (ClientId "xyz") (ClientSecret "abc"))
            decodeYAML (encodeYAML config) `shouldBe` Just config
