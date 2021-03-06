{-# OPTIONS -XEmptyDataDecls #-}
{-# OPTIONS -XKindSignatures #-}
{-# OPTIONS -XNoImplicitPrelude #-}
{-# OPTIONS -XFlexibleContexts #-}
{-# OPTIONS -XUndecidableInstances #-}
{-# OPTIONS -XStandaloneDeriving #-}
{-# OPTIONS -XOverloadedStrings #-}
{-# OPTIONS -XDeriveDataTypeable #-}
module Typeable.T205895c8d2df475b8d5ead5ee33d9f63 where
import Prelude
       (fromInteger, return, fail, undefined, (>>=), (>>), (==))
import qualified Prelude
import qualified Data.Tree
import qualified Data.Data
import qualified Data.Typeable
import qualified Data.Typeable.Extra
import qualified Data.Binary
import qualified Data.Binary.Put
import qualified Data.Binary.Get
import qualified Data.EBF
import Data.String
 
data Field (a :: *)
 
instance (Prelude.Eq a) => Prelude.Eq (Field a)
 
instance (Prelude.Ord a) => Prelude.Ord (Field a)
 
instance (Prelude.Show a) => Prelude.Show (Field a)
 
instance (Data.EBF.EBF a, Data.EBF.TypeIdent a) => Data.EBF.EBF
         (Field a)
 
instance Data.Typeable.Typeable1 Field
 
instance Data.EBF.TypeIdentS Field