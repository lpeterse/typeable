{-# OPTIONS -XEmptyDataDecls #-}
{-# OPTIONS -XKindSignatures #-}
{-# OPTIONS -XNoImplicitPrelude #-}
{-# OPTIONS -XFlexibleContexts #-}
{-# OPTIONS -XUndecidableInstances #-}
{-# OPTIONS -XStandaloneDeriving #-}
{-# OPTIONS -XOverloadedStrings #-}
module Typeable.Tbe46fbdd746743fe9ef39717c30b21fb where
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
import Data.String
 
data Application (a :: *) (b :: *)
 
instance (Prelude.Eq a, Prelude.Eq b) => Prelude.Eq
         (Application a b)
 
instance (Prelude.Ord a, Prelude.Ord b) => Prelude.Ord
         (Application a b)
 
instance (Prelude.Show a, Prelude.Show b) => Prelude.Show
         (Application a b)
 
instance (Data.EBF.EBF a, Data.EBF.EBF b, Data.EBF.TypeIdent a,
          Data.EBF.TypeIdent b) =>
         Data.EBF.EBF (Application a b)
 
instance Data.Typeable.Typeable2 Application
 
instance Data.EBF.TypeIdentSS Application