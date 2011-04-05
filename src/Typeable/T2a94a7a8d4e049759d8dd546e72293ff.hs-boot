{-# OPTIONS -XEmptyDataDecls #-}
{-# OPTIONS -XKindSignatures #-}
{-# OPTIONS -XNoImplicitPrelude #-}
{-# OPTIONS -XFlexibleContexts #-}
{-# OPTIONS -XUndecidableInstances #-}
{-# OPTIONS -XStandaloneDeriving #-}
{-# OPTIONS -XOverloadedStrings #-}
module Typeable.T2a94a7a8d4e049759d8dd546e72293ff where
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
 
data Constraint (a :: *)
 
instance (Prelude.Eq a) => Prelude.Eq (Constraint a)
 
instance (Prelude.Ord a) => Prelude.Ord (Constraint a)
 
instance (Prelude.Show a) => Prelude.Show (Constraint a)
 
instance (Data.EBF.EBF a, Data.EBF.TypeIdent a) => Data.EBF.EBF
         (Constraint a)
 
instance Data.Typeable.Typeable1 Constraint
 
instance Data.EBF.TypeIdentS Constraint