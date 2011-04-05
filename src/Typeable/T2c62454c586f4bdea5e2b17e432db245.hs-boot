{-# OPTIONS -XEmptyDataDecls #-}
{-# OPTIONS -XKindSignatures #-}
{-# OPTIONS -XNoImplicitPrelude #-}
{-# OPTIONS -XFlexibleContexts #-}
{-# OPTIONS -XUndecidableInstances #-}
{-# OPTIONS -XStandaloneDeriving #-}
module Typeable.T2c62454c586f4bdea5e2b17e432db245 where
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
 
data Extension (a :: *)
 
instance (Prelude.Eq a) => Prelude.Eq (Extension a)
 
instance (Prelude.Ord a) => Prelude.Ord (Extension a)
 
instance (Prelude.Show a) => Prelude.Show (Extension a)
 
instance (Data.EBF.EBF a) => Data.EBF.EBF (Extension a)
 
instance Data.Typeable.Typeable1 Extension
 
instance Data.EBF.TypeIdentS Extension