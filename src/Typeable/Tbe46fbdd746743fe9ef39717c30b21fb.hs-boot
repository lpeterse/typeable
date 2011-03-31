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
 
data Application (a :: *) (b :: *)
 
instance (Prelude.Eq a, Prelude.Eq b) => Prelude.Eq
         (Application a b)
 
instance (Prelude.Ord a, Prelude.Ord b) => Prelude.Ord
         (Application a b)
 
instance (Prelude.Show a, Prelude.Show b) => Prelude.Show
         (Application a b)
 
instance (Data.EBF.EBF a, Data.EBF.EBF b) => Data.EBF.EBF
         (Application a b)