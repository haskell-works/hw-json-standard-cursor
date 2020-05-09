{-# LANGUAGE ExplicitForAll            #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE InstanceSigs              #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE ScopedTypeVariables       #-}

{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module HaskellWorks.Data.Json.Standard.GenCursorTest(genTest) where

import Control.Monad
import HaskellWorks.Data.BalancedParens.BalancedParens
import HaskellWorks.Data.Bits.BitWise
-- import HaskellWorks.Data.Json.Internal.Index
-- import HaskellWorks.Data.Json.Internal.Standard.Cursor.Token
-- import HaskellWorks.Data.Json.Internal.Token
import HaskellWorks.Data.Json.Standard.Cursor.Generic
import HaskellWorks.Data.Positioning
import HaskellWorks.Data.RankSelect.Base.Rank0
import HaskellWorks.Data.RankSelect.Base.Rank1
import HaskellWorks.Data.RankSelect.Base.Select1
import HaskellWorks.Hspec.Hedgehog
import Hedgehog
import Test.Hspec

import qualified Data.ByteString              as BS
import qualified HaskellWorks.Data.TreeCursor as TC

{- HLINT ignore "Reduce duplication"  -}
{- HLINT ignore "Redundant do"        -}

fc = TC.firstChild
ns = TC.nextSibling
pn = TC.parent
ss = TC.subtreeSize

strAt :: forall t u.
  ( Eq                t
  , Show              t
  , Select1           t
  , Eq                u
  , Show              u
  , Rank0             u
  , Rank1             u
  , BalancedParens    u
  , TestBit           u)
  => Int
  -> GenericCursor BS.ByteString t u
  -> Maybe BS.ByteString
strAt n k = if balancedParens k .?. lastPositionOf (cursorRank k)
  then Just (BS.take n (BS.drop (fromIntegral (toCount (jsonCursorPos k))) (cursorText k)))
  else Nothing

genTest :: forall t u.
  ( Eq                t
  , Show              t
  , Select1           t
  , Eq                u
  , Show              u
  , Rank0             u
  , Rank1             u
  , BalancedParens    u
  , TestBit           u)
  => String -> (String -> GenericCursor BS.ByteString t u) -> SpecWith ()
genTest t mkCursor = do
  describe ("Json cursor of type " ++ t) $ do
    describe "For empty json array" $ do
      let cursor = mkCursor "[null]"
      it "can navigate down and forwards" $ requireTest $ do
        strAt 1 cursor === Just "["
    describe "For sample Json" $ do
      let cursor = mkCursor "\
            \{ \
            \    \"widget\": { \
            \        \"debug\": \"on\", \
            \        \"window\": { \
            \            \"name\": \"main_window\", \
            \            \"dimensions\": [500, 600.01e-02, true, false, null] \
            \        } \
            \    } \
            \}"
      it "can navigate up" $ requireTest $ do
        (                                                                      pn) cursor === Nothing
        (fc                                                                >=> pn) cursor ===                                    Just cursor
        (fc >=> ns                                                         >=> pn) cursor ===                                    Just cursor
        (fc >=> ns >=> fc                                                  >=> pn) cursor === (fc >=> ns                            ) cursor
        (fc >=> ns >=> fc >=> ns                                           >=> pn) cursor === (fc >=> ns                            ) cursor
        (fc >=> ns >=> fc >=> ns >=> ns                                    >=> pn) cursor === (fc >=> ns                            ) cursor
        (fc >=> ns >=> fc >=> ns >=> ns >=> ns                             >=> pn) cursor === (fc >=> ns                            ) cursor
        (fc >=> ns >=> fc >=> ns >=> ns >=> ns >=> fc                      >=> pn) cursor === (fc >=> ns >=> fc >=> ns >=> ns >=> ns) cursor
        (fc >=> ns >=> fc >=> ns >=> ns >=> ns >=> fc >=> ns               >=> pn) cursor === (fc >=> ns >=> fc >=> ns >=> ns >=> ns) cursor
        (fc >=> ns >=> fc >=> ns >=> ns >=> ns >=> fc >=> ns >=> ns        >=> pn) cursor === (fc >=> ns >=> fc >=> ns >=> ns >=> ns) cursor
        (fc >=> ns >=> fc >=> ns >=> ns >=> ns >=> fc >=> ns >=> ns >=> ns >=> pn) cursor === (fc >=> ns >=> fc >=> ns >=> ns >=> ns) cursor
      it "can get subtree size" $ requireTest $ do
        (                                                                      ss) cursor === Just 16
        (fc                                                                >=> ss) cursor === Just 1
        (fc >=> ns                                                         >=> ss) cursor === Just 14
        (fc >=> ns >=> fc                                                  >=> ss) cursor === Just 1
        (fc >=> ns >=> fc >=> ns                                           >=> ss) cursor === Just 1
        (fc >=> ns >=> fc >=> ns >=> ns                                    >=> ss) cursor === Just 1
        (fc >=> ns >=> fc >=> ns >=> ns >=> ns                             >=> ss) cursor === Just 10
        (fc >=> ns >=> fc >=> ns >=> ns >=> ns >=> fc                      >=> ss) cursor === Just 1
        (fc >=> ns >=> fc >=> ns >=> ns >=> ns >=> fc >=> ns               >=> ss) cursor === Just 1
        (fc >=> ns >=> fc >=> ns >=> ns >=> ns >=> fc >=> ns >=> ns        >=> ss) cursor === Just 1
        (fc >=> ns >=> fc >=> ns >=> ns >=> ns >=> fc >=> ns >=> ns >=> ns >=> ss) cursor === Just 6
      it "can get token at cursor" $ requireTest $ do
        (                                                                      strAt  1) cursor === Just "{"
        (fc                                                                >=> strAt  8) cursor === Just "\"widget\""
        (fc >=> ns                                                         >=> strAt  1) cursor === Just "{"
        (fc >=> ns >=> fc                                                  >=> strAt  7) cursor === Just "\"debug\""
        (fc >=> ns >=> fc >=> ns                                           >=> strAt  4) cursor === Just "\"on\""
        (fc >=> ns >=> fc >=> ns >=> ns                                    >=> strAt  8) cursor === Just "\"window\""
        (fc >=> ns >=> fc >=> ns >=> ns >=> ns                             >=> strAt  1) cursor === Just "{"
        (fc >=> ns >=> fc >=> ns >=> ns >=> ns >=> fc                      >=> strAt  6) cursor === Just "\"name\""
        (fc >=> ns >=> fc >=> ns >=> ns >=> ns >=> fc >=> ns               >=> strAt 13) cursor === Just "\"main_window\""
        (fc >=> ns >=> fc >=> ns >=> ns >=> ns >=> fc >=> ns >=> ns        >=> strAt 12) cursor === Just "\"dimensions\""
        (fc >=> ns >=> fc >=> ns >=> ns >=> ns >=> fc >=> ns >=> ns >=> ns >=> strAt  1) cursor === Just "["
