--------------------------------------------------
-- Copyright (C) 2018, All rights reserved.
--------------------------------------------------

module FitbitDemoSpec (spec) where

import           FitbitDemo
import           Test.Hspec

spec :: Spec
spec = do
    describe "sample" $
        it "runs" $ sample