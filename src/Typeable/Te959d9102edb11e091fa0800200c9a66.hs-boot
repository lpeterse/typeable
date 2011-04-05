{-# OPTIONS -XEmptyDataDecls #-}
{-# OPTIONS -XKindSignatures #-}
{-# OPTIONS -XNoImplicitPrelude #-}
{-# OPTIONS -XFlexibleContexts #-}
{-# OPTIONS -XUndecidableInstances #-}
{-# OPTIONS -XStandaloneDeriving #-}
module Typeable.Te959d9102edb11e091fa0800200c9a66 where
import Prelude
       (fromInteger, return, fail, undefined, (>>=), (>>), (==))
import qualified Prelude
import qualified Data.Typeable
import qualified Data.Typeable.Extra
import qualified Data.Binary
import qualified Data.Binary.Put
import qualified Data.Binary.Get
import qualified Data.EBF
 
data SimpleDialog (a :: *)
 
instance (Prelude.Eq a) => Prelude.Eq (SimpleDialog a)
 
instance (Prelude.Ord a) => Prelude.Ord (SimpleDialog a)
 
instance (Prelude.Show a) => Prelude.Show (SimpleDialog a)
 
instance (Data.EBF.EBF a) => Data.EBF.EBF (SimpleDialog a)
 
instance Data.Typeable.Typeable1 SimpleDialog