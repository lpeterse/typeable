{-# OPTIONS -XEmptyDataDecls #-}
{-# OPTIONS -XKindSignatures #-}
{-# OPTIONS -XNoImplicitPrelude #-}
{-# OPTIONS -XFlexibleContexts #-}
{-# OPTIONS -XUndecidableInstances #-}
{-# OPTIONS -XStandaloneDeriving #-}
module Typeable.Tf4b6d72c609d4003ba98917f8c56a678 where
import Prelude
       (fromInteger, return, fail, undefined, (>>=), (>>), (==))
import qualified Prelude
import qualified Data.Typeable
import qualified Data.Typeable.Extra
import qualified Data.Binary
import qualified Data.Binary.Put
import qualified Data.Binary.Get
import qualified Data.EBF
 
data Ordering
 
instance Prelude.Eq Ordering
 
instance Prelude.Ord Ordering
 
instance Prelude.Show Ordering
 
instance Data.EBF.EBF Ordering
 
instance Data.Typeable.Typeable Ordering
 
instance Prelude.Enum Ordering