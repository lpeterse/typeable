{-# OPTIONS -XEmptyDataDecls #-}
{-# OPTIONS -XKindSignatures #-}
{-# OPTIONS -XNoImplicitPrelude #-}
{-# OPTIONS -XFlexibleContexts #-}
{-# OPTIONS -XUndecidableInstances #-}
{-# OPTIONS -XStandaloneDeriving #-}
module Typeable.Tbe46fbdd746743fe9ef39717c30b21fb where
import Prelude
       (fromInteger, return, fail, undefined, (>>=), (>>), (==))
import qualified Prelude
import qualified Data.Binary
import qualified Data.Binary.Put
import qualified Data.Binary.Get
import qualified Data.EBF
 
data Application (a :: *) (b :: *) = First{}
                                   | Next{previous :: b}
 
deriving instance (Prelude.Eq b, Prelude.Eq a) => Prelude.Eq
         (Application a b)
 
deriving instance (Prelude.Ord b, Prelude.Ord a) => Prelude.Ord
         (Application a b)
 
deriving instance (Prelude.Show b, Prelude.Show a) => Prelude.Show
         (Application a b)
 
instance (Data.EBF.EBF b, Data.EBF.EBF a) => Data.EBF.EBF
         (Application a b) where
        get
          = do index <- Data.Binary.Get.getWord8
               case index of
                   0 -> return First
                   1 -> (>>=) Data.EBF.get (\ a0 -> return (Next a0))
        put First = do Data.Binary.Put.putWord8 0
        put (Next a)
          = do Data.Binary.Put.putWord8 1
               Data.EBF.put a