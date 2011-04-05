{-# OPTIONS -XEmptyDataDecls #-}
{-# OPTIONS -XKindSignatures #-}
{-# OPTIONS -XNoImplicitPrelude #-}
{-# OPTIONS -XFlexibleContexts #-}
{-# OPTIONS -XUndecidableInstances #-}
{-# OPTIONS -XStandaloneDeriving #-}
module Typeable.T606f253533d3420da3465afae341d598 where
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
 
data Time (a :: *)
 
instance (Prelude.Eq a) => Prelude.Eq (Time a)
 
instance (Prelude.Ord a) => Prelude.Ord (Time a)
 
instance (Prelude.Show a) => Prelude.Show (Time a)
 
instance (Data.EBF.EBF a) => Data.EBF.EBF (Time a)
 
instance Data.Typeable.Typeable1 Time
 
instance Data.EBF.TypeIdentS Time