{-# OPTIONS -XEmptyDataDecls #-}
{-# OPTIONS -XKindSignatures #-}
{-# OPTIONS -XNoImplicitPrelude #-}
{-# OPTIONS -XFlexibleContexts #-}
{-# OPTIONS -XUndecidableInstances #-}
{-# OPTIONS -XStandaloneDeriving #-}
module Typeable.T8068cbdaf35e4618a7e798c67ff9bee0 where
import Prelude
       (fromInteger, return, fail, undefined, (>>=), (>>), (==))
import qualified Prelude
import qualified Data.Binary
import qualified Data.Binary.Put
import qualified Data.Binary.Get
import qualified Typeable.Internal.EBF
 
data Hierarchy
 
instance Prelude.Eq Hierarchy
 
instance Prelude.Ord Hierarchy
 
instance Prelude.Show Hierarchy
 
instance Typeable.Internal.EBF.EBF Hierarchy