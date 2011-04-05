{-# OPTIONS -XEmptyDataDecls #-}
{-# OPTIONS -XKindSignatures #-}
{-# OPTIONS -XNoImplicitPrelude #-}
{-# OPTIONS -XFlexibleContexts #-}
{-# OPTIONS -XUndecidableInstances #-}
{-# OPTIONS -XStandaloneDeriving #-}
module Typeable.T0174bd2264004820bfe34e211cb35a7d where
import Prelude
       (fromInteger, return, fail, undefined, (>>=), (>>), (==))
import qualified Prelude
import qualified Data.Typeable
import qualified Data.Typeable.Extra
import qualified Data.Binary
import qualified Data.Binary.Put
import qualified Data.Binary.Get
import qualified Data.EBF
 
data DataType (a :: *)
 
instance (Prelude.Eq a) => Prelude.Eq (DataType a)
 
instance (Prelude.Ord a) => Prelude.Ord (DataType a)
 
instance (Prelude.Show a) => Prelude.Show (DataType a)
 
instance (Data.EBF.EBF a) => Data.EBF.EBF (DataType a)
 
instance Data.Typeable.Typeable1 DataType