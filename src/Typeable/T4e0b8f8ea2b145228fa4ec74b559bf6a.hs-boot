{-# OPTIONS -XEmptyDataDecls #-}
{-# OPTIONS -XKindSignatures #-}
{-# OPTIONS -XNoImplicitPrelude #-}
{-# OPTIONS -XFlexibleContexts #-}
{-# OPTIONS -XUndecidableInstances #-}
{-# OPTIONS -XStandaloneDeriving #-}
module Typeable.T4e0b8f8ea2b145228fa4ec74b559bf6a where
import Prelude
       (fromInteger, return, fail, undefined, (>>=), (>>), (==))
import qualified Prelude
import qualified Data.Typeable
import qualified Data.Typeable.Extra
import qualified Data.Binary
import qualified Data.Binary.Put
import qualified Data.Binary.Get
import qualified Data.EBF
 
data Class (a :: *)
 
instance (Prelude.Eq a) => Prelude.Eq (Class a)
 
instance (Prelude.Ord a) => Prelude.Ord (Class a)
 
instance (Prelude.Show a) => Prelude.Show (Class a)
 
instance (Data.EBF.EBF a) => Data.EBF.EBF (Class a)
 
instance Data.Typeable.Typeable1 Class