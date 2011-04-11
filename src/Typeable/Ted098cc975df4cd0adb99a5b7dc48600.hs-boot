{-# OPTIONS -XEmptyDataDecls #-}
{-# OPTIONS -XKindSignatures #-}
{-# OPTIONS -XNoImplicitPrelude #-}
{-# OPTIONS -XFlexibleContexts #-}
{-# OPTIONS -XUndecidableInstances #-}
{-# OPTIONS -XStandaloneDeriving #-}
{-# OPTIONS -XOverloadedStrings #-}
{-# OPTIONS -XDeriveDataTypeable #-}
module Typeable.Ted098cc975df4cd0adb99a5b7dc48600 where
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
 
data Mailto
 
instance Prelude.Eq Mailto
 
instance Prelude.Ord Mailto
 
instance Prelude.Show Mailto
 
instance Data.EBF.EBF Mailto
 
instance Data.Typeable.Typeable Mailto
 
instance Data.EBF.TypeIdent Mailto
 
instance Prelude.Enum Mailto