{-# LANGUAGE TypeFamilies #-}

module HaskellWorks.Data.Json.Standard.Cursor.Fast
  ( Cursor
  , fromByteString
  , fromForeignRegion
  , fromString
  ) where

import Foreign.ForeignPtr
import HaskellWorks.Data.Json.Standard.Cursor.Generic
import HaskellWorks.Data.Json.Standard.Cursor.Specific
import HaskellWorks.Data.RankSelect.CsPoppy

import qualified Data.ByteString                                      as BS
import qualified Data.ByteString.Char8                                as BSC
import qualified Data.ByteString.Internal                             as BSI
import qualified HaskellWorks.Data.BalancedParens.RangeMin            as RM
import qualified HaskellWorks.Data.FromForeignRegion                  as F
import qualified HaskellWorks.Data.Json.Standard.Cursor.Internal.IbBp as J

data Fast

instance SpecificCursor Fast where
  type CursorOf Fast = Cursor

type Cursor = GenericCursor BS.ByteString CsPoppy (RM.RangeMin CsPoppy)

fromByteString :: BS.ByteString -> Cursor
fromByteString bs = GenericCursor
  { cursorText      = bs
  , interests       = makeCsPoppy ib
  , balancedParens  = RM.mkRangeMin (makeCsPoppy bp)
  , cursorRank      = 1
  }
  where J.IbBp ib bp = J.slowToIbBp bs

fromForeignRegion :: F.ForeignRegion -> Cursor
fromForeignRegion (fptr, offset, size) = fromByteString (BSI.fromForeignPtr (castForeignPtr fptr) offset size)

fromString :: String -> Cursor
fromString = fromByteString . BSC.pack
