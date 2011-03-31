{-# OPTIONS -XEmptyDataDecls #-}
{-# OPTIONS -XKindSignatures #-}
{-# OPTIONS -XNoImplicitPrelude #-}
{-# OPTIONS -XFlexibleContexts #-}
{-# OPTIONS -XUndecidableInstances #-}
{-# OPTIONS -XStandaloneDeriving #-}
module Typeable.T2c62454c586f4bdea5e2b17e432db245 where
import Prelude
       (fromInteger, return, fail, undefined, (>>=), (>>), (==))
import qualified Prelude
import qualified Data.Binary
import qualified Data.Binary.Put
import qualified Data.Binary.Get
import qualified Data.EBF
 
data Extension (a :: *)
 
instance (Prelude.Eq a) => Prelude.Eq (Extension a)
 
instance (Prelude.Ord a) => Prelude.Ord (Extension a)
 
instance (Prelude.Show a) => Prelude.Show (Extension a)
 
instance (Data.EBF.EBF a) => Data.EBF.EBF (Extension a)