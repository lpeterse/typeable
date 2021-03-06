{-# OPTIONS -XEmptyDataDecls #-}
{-# OPTIONS -XKindSignatures #-}
{-# OPTIONS -XNoImplicitPrelude #-}
{-# OPTIONS -XFlexibleContexts #-}
{-# OPTIONS -XUndecidableInstances #-}
{-# OPTIONS -XStandaloneDeriving #-}
{-# OPTIONS -XOverloadedStrings #-}
{-# OPTIONS -XDeriveDataTypeable #-}
module Typeable.T10f280df659654becb6e08122e846284 where
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
 
data Unit
 
instance Prelude.Eq Unit
 
instance Prelude.Ord Unit
 
instance Prelude.Show Unit
 
instance Data.EBF.EBF Unit
 
instance Data.Typeable.Typeable Unit
 
instance Data.EBF.TypeIdent Unit
 
instance Prelude.Enum Unit