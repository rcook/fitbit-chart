module Lib.Util.Format
    ( formatDay
    ) where

import           Data.Text (Text)
import qualified Data.Text as Text (pack)
import           Data.Time.Calendar (Day)

formatDay :: Day -> Text
formatDay = Text.pack . show
