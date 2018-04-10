module App.Log
    ( logInfo
    ) where

import           System.IO (hFlush, stdout)

logInfo :: String -> IO ()
logInfo s = do
    putStrLn $ "[info] " ++ s
    hFlush stdout
