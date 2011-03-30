{-# OPTIONS -XFlexibleInstances #-}
module Typeable.Tbbabbac1510d49aa9da25d8033147c54 (Word128) where

import Data.Word
import Data.LargeWord
import Data.Binary
import Data.Binary.Put
import Data.Binary.Get
import Typeable.Internal.EBF

instance Read (LargeKey Word64 Word64) where
  readsPrec = error "Word128: no Read instance" 

instance EBF (LargeKey Word64 Word64) where
  put x = putWord64be (hiHalf x) >> putWord64be (loHalf x)
  get = getWord64be >>= \h-> getWord64be >>= \l-> return (fromIntegral (((fromIntegral h)*2^64+(fromIntegral l))::Integer))
