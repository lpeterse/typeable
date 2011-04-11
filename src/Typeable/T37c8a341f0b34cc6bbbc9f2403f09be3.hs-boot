{-# OPTIONS -XEmptyDataDecls #-}
{-# OPTIONS -XKindSignatures #-}
{-# OPTIONS -XNoImplicitPrelude #-}
{-# OPTIONS -XFlexibleContexts #-}
{-# OPTIONS -XUndecidableInstances #-}
{-# OPTIONS -XStandaloneDeriving #-}
{-# OPTIONS -XOverloadedStrings #-}
{-# OPTIONS -XDeriveDataTypeable #-}
module Typeable.T37c8a341f0b34cc6bbbc9f2403f09be3 where
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
 
data Constructor (a :: *)
 
instance (Prelude.Eq a) => Prelude.Eq (Constructor a)
 
instance (Prelude.Ord a) => Prelude.Ord (Constructor a)
 
instance (Prelude.Show a) => Prelude.Show (Constructor a)
 
instance (Data.EBF.EBF a, Data.EBF.TypeIdent a) => Data.EBF.EBF
         (Constructor a)
 
instance Data.Typeable.Typeable1 Constructor
 
instance Data.EBF.TypeIdentS Constructor