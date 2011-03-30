{-# OPTIONS -XEmptyDataDecls #-}
{-# OPTIONS -XKindSignatures #-}
{-# OPTIONS -XNoImplicitPrelude #-}
{-# OPTIONS -XFlexibleContexts #-}
{-# OPTIONS -XUndecidableInstances #-}
{-# OPTIONS -XStandaloneDeriving #-}
module Typeable.Tf4b6d72c609d4003ba98917f8c56a678 where
import Prelude
       (fromInteger, return, fail, undefined, (>>=), (>>), (==))
import qualified Prelude
import qualified Data.Binary
import qualified Data.Binary.Put
import qualified Data.Binary.Get
import qualified Typeable.Internal.EBF
 
data Ordering = LT{}
              | EQ{}
              | GT{}
 
deriving instance Prelude.Eq Ordering
 
deriving instance Prelude.Ord Ordering
 
deriving instance Prelude.Show Ordering
 
instance Typeable.Internal.EBF.EBF Ordering where
        get
          = do index <- Data.Binary.Get.getWord8
               case index of
                   0 -> return LT
                   1 -> return EQ
                   2 -> return GT
        put LT = do Data.Binary.Put.putWord8 0
        put EQ = do Data.Binary.Put.putWord8 1
        put GT = do Data.Binary.Put.putWord8 2
 
deriving instance Prelude.Enum Ordering