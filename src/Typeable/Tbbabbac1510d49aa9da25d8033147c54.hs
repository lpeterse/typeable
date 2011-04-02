{-# OPTIONS -XFlexibleInstances #-}
module Typeable.Tbbabbac1510d49aa9da25d8033147c54 (Word128) where

import Data.Word
import Data.LargeWord

instance Read (LargeKey Word64 Word64) where
  readsPrec = error "Word128: no Read instance" 

