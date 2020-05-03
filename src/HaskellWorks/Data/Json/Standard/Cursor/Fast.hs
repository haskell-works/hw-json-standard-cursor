{-# LANGUAGE TypeFamilies #-}

module HaskellWorks.Data.Json.Standard.Cursor.Fast
  ( Cursor
  , fromByteString
  , fromByteStringViaBlanking
  , fromByteStringViaSimd
  , fromForeignRegion
  , fromString
  , fromBsIbBp
  , simdToIbBp
  ) where

import Foreign.ForeignPtr
import HaskellWorks.Data.Json.Standard.Cursor.Generic
import HaskellWorks.Data.Json.Standard.Cursor.IbBp
import HaskellWorks.Data.Json.Standard.Cursor.Specific
import HaskellWorks.Data.RankSelect.CsPoppy1

import qualified Data.ByteString                             as BS
import qualified Data.ByteString.Char8                       as BSC
import qualified Data.ByteString.Internal                    as BSI
import qualified HaskellWorks.Data.BalancedParens.RangeMin   as RM
import qualified HaskellWorks.Data.FromForeignRegion         as F
import qualified HaskellWorks.Data.Json.Standard.Cursor.IbBp as J

data Fast

instance SpecificCursor Fast where
  type CursorOf Fast = Cursor

type Cursor = GenericCursor BS.ByteString CsPoppy1 (RM.RangeMin CsPoppy1)

fromBsIbBp :: BS.ByteString -> IbBp -> Cursor
fromBsIbBp bs ibBp = GenericCursor
  { cursorText      = bs
  , interests       = makeCsPoppy ibPart
  , balancedParens  = RM.mkRangeMin (makeCsPoppy bpPart)
  , cursorRank      = 1
  }
  where J.IbBp ibPart bpPart = ibBp

-- | Load a 'Cursor' from a 'ByteString'
fromByteString :: BS.ByteString -> Cursor
fromByteString = fromByteStringViaBlanking
{-# DEPRECATED fromByteString "Use one of the other fromByteString* functions" #-}

-- | Load a 'Cursor' from a 'ByteString' via the blanking process.
-- This has reasonable performance, but uses a lot of memory due to
-- the lack of streaming
fromByteStringViaBlanking :: BS.ByteString -> Cursor
fromByteStringViaBlanking bs = fromBsIbBp bs (J.slowToIbBp bs)

-- | Load a 'Cursor' from a 'ByteString' via the blanking via `simd`
-- This has fast performance and streaming, but is only available
-- recent x86 platforms
fromByteStringViaSimd :: BS.ByteString -> Cursor
fromByteStringViaSimd jsonBs = fromBsIbBp jsonBs (simdToIbBp jsonBs)

fromForeignRegion :: F.ForeignRegion -> Cursor
fromForeignRegion (fptr, offset, size) = fromByteString (BSI.fromForeignPtr (castForeignPtr fptr) offset size)

fromString :: String -> Cursor
fromString = fromByteString . BSC.pack
