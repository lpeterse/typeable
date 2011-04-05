{-# OPTIONS -XEmptyDataDecls #-}
{-# OPTIONS -XKindSignatures #-}
{-# OPTIONS -XNoImplicitPrelude #-}
{-# OPTIONS -XFlexibleContexts #-}
{-# OPTIONS -XUndecidableInstances #-}
{-# OPTIONS -XStandaloneDeriving #-}
module Typeable.Td9eef038b47d0820c160ceb8b6a89943 where
import Prelude
       (fromInteger, return, fail, undefined, (>>=), (>>), (==))
import qualified Prelude
import qualified Data.Typeable
import qualified Data.Typeable.Extra
import qualified Data.Binary
import qualified Data.Binary.Put
import qualified Data.Binary.Get
import qualified Data.EBF
 
data Either (a :: *) (b :: *)
 
instance (Prelude.Eq a, Prelude.Eq b) => Prelude.Eq (Either a b)
 
instance (Prelude.Ord a, Prelude.Ord b) => Prelude.Ord (Either a b)
 
instance (Prelude.Show a, Prelude.Show b) => Prelude.Show
         (Either a b)
 
instance (Data.EBF.EBF a, Data.EBF.EBF b) => Data.EBF.EBF
         (Either a b)
 
instance Data.Typeable.Typeable2 Either