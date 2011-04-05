{-# OPTIONS -XEmptyDataDecls #-}
{-# OPTIONS -XKindSignatures #-}
{-# OPTIONS -XNoImplicitPrelude #-}
{-# OPTIONS -XFlexibleContexts #-}
{-# OPTIONS -XUndecidableInstances #-}
{-# OPTIONS -XStandaloneDeriving #-}
module Typeable.Tb0221a43509e4eddb062101bfd794bc4 where
import Prelude
       (fromInteger, return, fail, undefined, (>>=), (>>), (==))
import qualified Prelude
import qualified Data.Tree
import qualified Data.Typeable
import qualified Data.Typeable.Extra
import qualified Data.Binary
import qualified Data.Binary.Put
import qualified Data.Binary.Get
import qualified Data.EBF
import qualified Typeable.T346674042a7248b4a94abff0726d0c43 as UUID
 
data StructuredText (a :: *)
 
instance (Prelude.Eq a) => Prelude.Eq (StructuredText a)
 
instance (Prelude.Ord a) => Prelude.Ord (StructuredText a)
 
instance (Prelude.Show a) => Prelude.Show (StructuredText a)
 
instance (Data.EBF.EBF a) => Data.EBF.EBF (StructuredText a)
 
instance Data.Typeable.Typeable1 StructuredText
 
instance Data.EBF.TypeIdentS StructuredText