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
import qualified Data.Binary
import qualified Data.Binary.Put
import qualified Data.Binary.Get
import qualified Typeable.Internal.EBF
 
data DataType (a :: *)
 
instance (Prelude.Eq a) => Prelude.Eq (DataType a)
 
instance (Prelude.Ord a) => Prelude.Ord (DataType a)
 
instance (Prelude.Show a) => Prelude.Show (DataType a)
 
instance (Typeable.Internal.EBF.EBF a) => Typeable.Internal.EBF.EBF
         (DataType a)