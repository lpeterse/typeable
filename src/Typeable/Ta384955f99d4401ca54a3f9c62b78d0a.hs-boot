{-# OPTIONS -XEmptyDataDecls #-}
{-# OPTIONS -XKindSignatures #-}
{-# OPTIONS -XNoImplicitPrelude #-}
{-# OPTIONS -XFlexibleContexts #-}
{-# OPTIONS -XUndecidableInstances #-}
{-# OPTIONS -XStandaloneDeriving #-}
{-# OPTIONS -XOverloadedStrings #-}
module Typeable.Ta384955f99d4401ca54a3f9c62b78d0a where
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
 
data Numerus
 
instance Prelude.Eq Numerus
 
instance Prelude.Ord Numerus
 
instance Prelude.Show Numerus
 
instance Data.EBF.EBF Numerus
 
instance Data.Typeable.Typeable Numerus
 
instance Data.EBF.TypeIdent Numerus
 
instance Prelude.Enum Numerus