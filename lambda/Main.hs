module Main (main) where

import           App.Log (logInfo)
import           Util

main :: IO ()
main = withLambda $ \_ -> do
    logInfo "Done"
