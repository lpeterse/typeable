{-# OPTIONS -XEmptyDataDecls #-}
{-# OPTIONS -XKindSignatures #-}
{-# OPTIONS -XNoImplicitPrelude #-}
{-# OPTIONS -XFlexibleContexts #-}
{-# OPTIONS -XUndecidableInstances #-}
{-# OPTIONS -XStandaloneDeriving #-}
{-# OPTIONS -XOverloadedStrings #-}
{-# OPTIONS -XDeriveDataTypeable #-}
module Typeable.T8068cbdaf35e4618a7e798c67ff9bee0 where
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
 
data Hierarchy
 
instance Prelude.Eq Hierarchy
 
instance Prelude.Ord Hierarchy
 
instance Prelude.Show Hierarchy
 
instance Data.EBF.EBF Hierarchy
 
instance Data.Typeable.Typeable Hierarchy
 
instance Data.EBF.TypeIdent Hierarchy