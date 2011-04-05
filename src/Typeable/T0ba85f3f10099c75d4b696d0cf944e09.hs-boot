{-# OPTIONS -XEmptyDataDecls #-}
{-# OPTIONS -XKindSignatures #-}
{-# OPTIONS -XNoImplicitPrelude #-}
{-# OPTIONS -XFlexibleContexts #-}
{-# OPTIONS -XUndecidableInstances #-}
{-# OPTIONS -XStandaloneDeriving #-}
module Typeable.T0ba85f3f10099c75d4b696d0cf944e09 where
import Prelude
       (fromInteger, return, fail, undefined, (>>=), (>>), (==))
import qualified Prelude
import qualified Data.Typeable
import qualified Data.Typeable.Extra
import qualified Data.Binary
import qualified Data.Binary.Put
import qualified Data.Binary.Get
import qualified Data.EBF
 
data List (a :: *)
 
instance (Prelude.Eq a) => Prelude.Eq (List a)
 
instance (Prelude.Ord a) => Prelude.Ord (List a)
 
instance (Prelude.Show a) => Prelude.Show (List a)
 
instance (Data.EBF.EBF a) => Data.EBF.EBF (List a)
 
instance Data.Typeable.Typeable1 List