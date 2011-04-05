{-# OPTIONS -XEmptyDataDecls #-}
{-# OPTIONS -XKindSignatures #-}
{-# OPTIONS -XNoImplicitPrelude #-}
{-# OPTIONS -XFlexibleContexts #-}
{-# OPTIONS -XUndecidableInstances #-}
{-# OPTIONS -XStandaloneDeriving #-}
module Typeable.T6ffbfb8682ad4f6a89d73e6d36c8fc7a where
import Prelude
       (fromInteger, return, fail, undefined, (>>=), (>>), (==))
import qualified Prelude
import qualified Data.Typeable
import qualified Data.Typeable.Extra
import qualified Data.Binary
import qualified Data.Binary.Put
import qualified Data.Binary.Get
import qualified Data.EBF
 
data AbsolutePath
 
instance Prelude.Eq AbsolutePath
 
instance Prelude.Ord AbsolutePath
 
instance Prelude.Show AbsolutePath
 
instance Data.EBF.EBF AbsolutePath
 
instance Data.Typeable.Typeable AbsolutePath