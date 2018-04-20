{-|
Module      : FitbitChart.S3
Description : S3 helpers
Copyright   : (C) Richard Cook, 2018
Licence     : MIT
Maintainer  : rcook@rcook.org
Stability   : stable
Portability : portable

S3 helpers
-}

module FitbitChart.S3
    ( putBytes
    ) where

import           Control.Monad (void)
import           Data.ByteString.Lazy (ByteString)
import           FitbitChart.AWS
import           Network.AWS (send)
import           Network.AWS.Data.Body
                    ( RqBody(..)
                    , ToHashedBody(..)
                    )
import           Network.AWS.Easy (withAWS)
import           Network.AWS.S3
                    ( BucketName(..)
                    , ObjectKey(..)
                    , putObject
                    )

putBytes :: BucketName -> ObjectKey -> ByteString -> S3Session -> IO ()
putBytes bucketName objectKey bytes = withAWS $ do
    void $ send $ putObject bucketName objectKey (Hashed $ toHashed bytes)
