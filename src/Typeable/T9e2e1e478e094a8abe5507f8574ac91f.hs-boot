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
import qualified Data.EBF
 
data Succ (a :: *)
 
instance (Prelude.Eq a) => Prelude.Eq (Succ a)
 
instance (Prelude.Ord a) => Prelude.Ord (Succ a)
 
instance (Prelude.Show a) => Prelude.Show (Succ a)
 
instance (Data.EBF.EBF a) => Data.EBF.EBF (Succ a)