{-# OPTIONS -XEmptyDataDecls #-}
{-# OPTIONS -XKindSignatures #-}
{-# OPTIONS -XNoImplicitPrelude #-}
{-# OPTIONS -XFlexibleContexts #-}
{-# OPTIONS -XUndecidableInstances #-}
{-# OPTIONS -XStandaloneDeriving #-}
module Typeable.T451f847e1cb642d0b7c5dbdfa03f41b5 where
import Prelude
       (fromInteger, return, fail, undefined, (>>=), (>>), (==))
import qualified Prelude
import qualified Data.Binary
import qualified Data.Binary.Put
import qualified Data.Binary.Get
import qualified Typeable.Internal.EBF
 
data Definition (a :: * -> *)
 
instance Prelude.Eq (Definition a)
 
instance Prelude.Ord (Definition a)
 
instance Prelude.Show (Definition a)
 
instance Typeable.Internal.EBF.EBF (Definition a)