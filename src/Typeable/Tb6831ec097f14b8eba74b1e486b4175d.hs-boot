{-# OPTIONS -XEmptyDataDecls #-}
{-# OPTIONS -XKindSignatures #-}
{-# OPTIONS -XNoImplicitPrelude #-}
{-# OPTIONS -XFlexibleContexts #-}
{-# OPTIONS -XUndecidableInstances #-}
{-# OPTIONS -XStandaloneDeriving #-}
module Typeable.Tb6831ec097f14b8eba74b1e486b4175d where
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
 
data Date (a :: *)
 
instance (Prelude.Eq a) => Prelude.Eq (Date a)
 
instance (Prelude.Ord a) => Prelude.Ord (Date a)
 
instance (Prelude.Show a) => Prelude.Show (Date a)
 
instance (Data.EBF.EBF a) => Data.EBF.EBF (Date a)
 
instance Data.Typeable.Typeable1 Date
 
instance Data.EBF.TypeIdentS Date