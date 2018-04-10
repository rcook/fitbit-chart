module Main (main) where

import           App.Log (logInfo)
import           Util

main :: IO ()
main = withLambda $ \event -> do
    print event
    logInfo "Done"
