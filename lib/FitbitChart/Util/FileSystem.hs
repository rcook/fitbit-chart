module FitbitChart.Util.FileSystem
    ( expandPath
    ) where

import           System.Directory (canonicalizePath, getHomeDirectory)
import           System.FilePath ((</>))

expandPath :: FilePath -> IO FilePath
expandPath ('~' : '/' : cs) = do
    homeDir <- getHomeDirectory
    return $ homeDir </> cs
expandPath p = canonicalizePath p
