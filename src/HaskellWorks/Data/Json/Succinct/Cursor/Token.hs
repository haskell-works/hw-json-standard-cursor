
module HaskellWorks.Data.Json.Succinct.Cursor.Token
  ( jsonTokenAt
  ) where

import qualified Data.Attoparsec.ByteString.Char8                 as ABC
import           Data.ByteString.Internal                         as BSI
import           HaskellWorks.Data.Bits.BitWise
import           HaskellWorks.Data.Drop
import           HaskellWorks.Data.Json.Succinct.Cursor.Internal
import           HaskellWorks.Data.Json.Token.Tokenize
import           HaskellWorks.Data.Positioning
import           HaskellWorks.Data.RankSelect.Base.Rank1
import           HaskellWorks.Data.RankSelect.Base.Select1
import           Prelude hiding (drop)

jsonTokenAt :: (Rank1 w, Select1 v, TestBit w) => JsonCursor ByteString v w -> Maybe (JsonToken String Double)
jsonTokenAt k = if balancedParens k .?. lastPositionOf (cursorRank k)
  then case ABC.parse parseJsonToken (drop (toCount (jsonCursorPos k)) (cursorText k)) of
    ABC.Fail    {}  -> error "Failed to parse token in cursor"
    ABC.Partial _   -> error "Failed to parse token in cursor"
    ABC.Done    _ r -> Just r
  else Nothing
