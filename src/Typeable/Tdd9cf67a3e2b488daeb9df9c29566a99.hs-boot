{-# OPTIONS -XEmptyDataDecls #-}
{-# OPTIONS -XKindSignatures #-}
{-# OPTIONS -XNoImplicitPrelude #-}
{-# OPTIONS -XFlexibleContexts #-}
{-# OPTIONS -XUndecidableInstances #-}
{-# OPTIONS -XStandaloneDeriving #-}
module Typeable.Tdd9cf67a3e2b488daeb9df9c29566a99 where
import Prelude
       (fromInteger, return, fail, undefined, (>>=), (>>), (==))
import qualified Prelude
import qualified Data.Typeable
import qualified Data.Typeable.Extra
import qualified Data.Binary
import qualified Data.Binary.Put
import qualified Data.Binary.Get
import qualified Data.EBF
 
data Turn (a :: *)
 
instance (Prelude.Eq a) => Prelude.Eq (Turn a)
 
instance (Prelude.Ord a) => Prelude.Ord (Turn a)
 
instance (Prelude.Show a) => Prelude.Show (Turn a)
 
instance (Data.EBF.EBF a) => Data.EBF.EBF (Turn a)
 
instance Data.Typeable.Typeable1 Turn