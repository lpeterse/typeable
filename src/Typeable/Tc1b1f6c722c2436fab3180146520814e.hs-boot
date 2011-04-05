{-# OPTIONS -XEmptyDataDecls #-}
{-# OPTIONS -XKindSignatures #-}
{-# OPTIONS -XNoImplicitPrelude #-}
{-# OPTIONS -XFlexibleContexts #-}
{-# OPTIONS -XUndecidableInstances #-}
{-# OPTIONS -XStandaloneDeriving #-}
module Typeable.Tc1b1f6c722c2436fab3180146520814e where
import Prelude
       (fromInteger, return, fail, undefined, (>>=), (>>), (==))
import qualified Prelude
import qualified Data.Typeable
import qualified Data.Typeable.Extra
import qualified Data.Binary
import qualified Data.Binary.Put
import qualified Data.Binary.Get
import qualified Data.EBF
 
data UTC
 
instance Prelude.Eq UTC
 
instance Prelude.Ord UTC
 
instance Prelude.Show UTC
 
instance Data.EBF.EBF UTC
 
instance Data.Typeable.Typeable UTC
 
instance Prelude.Enum UTC