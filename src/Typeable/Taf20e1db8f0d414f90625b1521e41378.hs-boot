{-# OPTIONS -XEmptyDataDecls #-}
{-# OPTIONS -XKindSignatures #-}
{-# OPTIONS -XNoImplicitPrelude #-}
{-# OPTIONS -XFlexibleContexts #-}
{-# OPTIONS -XUndecidableInstances #-}
{-# OPTIONS -XStandaloneDeriving #-}
{-# OPTIONS -XOverloadedStrings #-}
module Typeable.Taf20e1db8f0d414f90625b1521e41378 where
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
 
data Language
 
instance Prelude.Eq Language
 
instance Prelude.Ord Language
 
instance Prelude.Show Language
 
instance Data.EBF.EBF Language
 
instance Data.Typeable.Typeable Language
 
instance Data.EBF.TypeIdent Language
 
instance Prelude.Enum Language