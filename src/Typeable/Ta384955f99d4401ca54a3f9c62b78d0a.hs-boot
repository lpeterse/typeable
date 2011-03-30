{-# OPTIONS -XEmptyDataDecls #-}
{-# OPTIONS -XKindSignatures #-}
{-# OPTIONS -XNoImplicitPrelude #-}
{-# OPTIONS -XFlexibleContexts #-}
{-# OPTIONS -XUndecidableInstances #-}
{-# OPTIONS -XStandaloneDeriving #-}
module Typeable.Ta384955f99d4401ca54a3f9c62b78d0a where
import Prelude
       (fromInteger, return, fail, undefined, (>>=), (>>), (==))
import qualified Prelude
import qualified Data.Binary
import qualified Data.Binary.Put
import qualified Data.Binary.Get
import qualified Typeable.Internal.EBF
 
data Numerus
 
instance Prelude.Eq Numerus
 
instance Prelude.Ord Numerus
 
instance Prelude.Show Numerus
 
instance Typeable.Internal.EBF.EBF Numerus
 
instance Prelude.Enum Numerus