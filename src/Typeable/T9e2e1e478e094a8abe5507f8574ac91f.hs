{-# OPTIONS -XEmptyDataDecls #-}
{-# OPTIONS -XKindSignatures #-}
{-# OPTIONS -XNoImplicitPrelude #-}
{-# OPTIONS -XFlexibleContexts #-}
{-# OPTIONS -XUndecidableInstances #-}
{-# OPTIONS -XStandaloneDeriving #-}
module Typeable.T9e2e1e478e094a8abe5507f8574ac91f where
import Prelude
       (fromInteger, return, fail, undefined, (>>=), (>>), (==))
import qualified Prelude
import qualified Data.Binary
import qualified Data.Binary.Put
import qualified Data.Binary.Get
import qualified Typeable.Internal.EBF
 
data Succ (a :: *) = First{}
                   | Next{previous :: a}
 
deriving instance (Prelude.Eq a) => Prelude.Eq (Succ a)
 
deriving instance (Prelude.Ord a) => Prelude.Ord (Succ a)
 
deriving instance (Prelude.Show a) => Prelude.Show (Succ a)
 
instance (Typeable.Internal.EBF.EBF a) => Typeable.Internal.EBF.EBF
         (Succ a) where
        get
          = do index <- Data.Binary.Get.getWord8
               case index of
                   0 -> return First
                   1 -> (>>=) Typeable.Internal.EBF.get (\ a0 -> return (Next a0))
        put First = do Data.Binary.Put.putWord8 0
        put (Next a)
          = do Data.Binary.Put.putWord8 1
               Typeable.Internal.EBF.put a