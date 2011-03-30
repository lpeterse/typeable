{-# OPTIONS -XEmptyDataDecls #-}
{-# OPTIONS -XKindSignatures #-}
{-# OPTIONS -XNoImplicitPrelude #-}
{-# OPTIONS -XFlexibleContexts #-}
{-# OPTIONS -XUndecidableInstances #-}
{-# OPTIONS -XStandaloneDeriving #-}
module Typeable.Tf18ae792e5324a68a16f11ea5c61442a where
import Prelude
       (fromInteger, return, fail, undefined, (>>=), (>>), (==))
import qualified Prelude
import qualified Data.Binary
import qualified Data.Binary.Put
import qualified Data.Binary.Get
import qualified Typeable.Internal.EBF
 
data Tel
 
instance Prelude.Eq Tel
 
instance Prelude.Ord Tel
 
instance Prelude.Show Tel
 
instance Typeable.Internal.EBF.EBF Tel
 
instance Prelude.Enum Tel