{-# OPTIONS -XFlexibleInstances -XDeriveDataTypeable -XStandaloneDeriving  #-}
module Typeable.Tbbabbac1510d49aa9da25d8033147c54 (Word128) where

import Data.Word
import Data.LargeWord
import Data.Typeable
import Data.Data

instance Read (LargeKey Word64 Word64) where
  readsPrec = error "Word128: no Read instance" 

instance Typeable2 LargeKey where
  typeOf2 = undefined

instance Data (LargeKey Word64 Word64) where
  gfoldl      = error "Word128: no instance of Data.Data"
  toConstr    = error "Word128: no instance of Data.Data"
  gunfold     = error "Word128: no instance of Data.Data"
  dataTypeOf  = error "Word128: no instance of Data.Data"
  dataCast1   = error "Word128: no instance of Data.Data"
