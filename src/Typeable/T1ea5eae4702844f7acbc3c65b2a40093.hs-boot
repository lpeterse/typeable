{-# OPTIONS -XEmptyDataDecls #-}
{-# OPTIONS -XKindSignatures #-}
{-# OPTIONS -XNoImplicitPrelude #-}
{-# OPTIONS -XFlexibleContexts #-}
{-# OPTIONS -XUndecidableInstances #-}
{-# OPTIONS -XStandaloneDeriving #-}
module Typeable.T1ea5eae4702844f7acbc3c65b2a40093 where
import Prelude
       (fromInteger, return, fail, undefined, (>>=), (>>), (==))
import qualified Prelude
import qualified Data.Typeable
import qualified Data.Typeable.Extra
import qualified Data.Binary
import qualified Data.Binary.Put
import qualified Data.Binary.Get
import qualified Data.EBF
 
data UriByScheme (a :: *)
 
instance (Prelude.Eq a) => Prelude.Eq (UriByScheme a)
 
instance (Prelude.Ord a) => Prelude.Ord (UriByScheme a)
 
instance (Prelude.Show a) => Prelude.Show (UriByScheme a)
 
instance (Data.EBF.EBF a) => Data.EBF.EBF (UriByScheme a)
 
instance Data.Typeable.Typeable1 UriByScheme