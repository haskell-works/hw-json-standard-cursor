{-# LANGUAGE DeriveGeneric #-}

module HaskellWorks.Data.Json.Standard.Cursor.IbBp
  ( IbBp(..)
  , slowToIbBp
  , simdToIbBp
  ) where

import Control.Monad
import Control.Monad.ST                    (ST)
import Data.Word
import GHC.Generics
import HaskellWorks.Data.Vector.AsVector64

import qualified Data.ByteString                                                    as BS
import qualified Data.ByteString.Lazy                                               as LBS
import qualified Data.Vector.Storable                                               as DVS
import qualified Data.Vector.Storable.Mutable                                       as DVSM
import qualified HaskellWorks.Data.ByteString.Lazy                                  as LBS
import qualified HaskellWorks.Data.Json.Simd.Index.Standard                         as STSI
import qualified HaskellWorks.Data.Json.Standard.Cursor.Internal.BlankedJson        as J
import qualified HaskellWorks.Data.Json.Standard.Cursor.Internal.ToBalancedParens64 as J
import qualified HaskellWorks.Data.Json.Standard.Cursor.Internal.ToInterestBits64   as J
import qualified HaskellWorks.Data.Vector.Storable                                  as DVS

data IbBp = IbBp
  { ib :: DVS.Vector Word64
  , bp :: DVS.Vector Word64
  } deriving Generic

slowToIbBp :: BS.ByteString -> IbBp
slowToIbBp bs = IbBp
  { ib = J.toInterestBits64 blankedJson
  , bp = J.toBalancedParens64 blankedJson
  }
  where blankedJson = J.toBlankedJsonTyped bs

simdToIbBp :: BS.ByteString -> IbBp
simdToIbBp bs = case STSI.makeStandardJsonIbBps (LBS.rechunkPadded chunkSize (LBS.fromStrict bs)) of
  Left msg     -> error msg
  Right chunks -> uncurry IbBp $ DVS.construct2N maxSize (go . fst) maxSize (go . snd) chunks
  where chunkSize = 16384
        maxSize = (BS.length bs + chunkSize - 1) `div` 8
        go :: BS.ByteString -> DVSM.MVector s Word64 -> ST s Int
        go bytes mv = do
          let source = asVector64 bytes
          let target = DVSM.take (DVS.length source) mv
          when (DVS.length source > 0) $ DVS.copy target source
          return (DVS.length source)
