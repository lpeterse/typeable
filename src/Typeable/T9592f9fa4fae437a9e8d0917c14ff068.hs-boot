{-# OPTIONS -XEmptyDataDecls #-}
{-# OPTIONS -XKindSignatures #-}
{-# OPTIONS -XNoImplicitPrelude #-}
{-# OPTIONS -XFlexibleContexts #-}
{-# OPTIONS -XUndecidableInstances #-}
{-# OPTIONS -XStandaloneDeriving #-}
module Typeable.T9592f9fa4fae437a9e8d0917c14ff068 where
import Prelude
       (fromInteger, return, fail, undefined, (>>=), (>>), (==))
import qualified Prelude
import qualified Data.Binary
import qualified Data.Binary.Put
import qualified Data.Binary.Get
import qualified Typeable.Internal.EBF
 
data TextElement (a :: *)
 
instance (Prelude.Eq a) => Prelude.Eq (TextElement a)
 
instance (Prelude.Ord a) => Prelude.Ord (TextElement a)
 
instance (Prelude.Show a) => Prelude.Show (TextElement a)
 
instance (Typeable.Internal.EBF.EBF a) => Typeable.Internal.EBF.EBF
         (TextElement a)