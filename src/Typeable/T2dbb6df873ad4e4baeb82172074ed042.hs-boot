{-# OPTIONS -XEmptyDataDecls #-}
{-# OPTIONS -XKindSignatures #-}
{-# OPTIONS -XNoImplicitPrelude #-}
{-# OPTIONS -XFlexibleContexts #-}
{-# OPTIONS -XUndecidableInstances #-}
{-# OPTIONS -XStandaloneDeriving #-}
module Typeable.T2dbb6df873ad4e4baeb82172074ed042 where
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
import qualified Typeable.T346674042a7248b4a94abff0726d0c43 as UUID
 
data Gender
 
instance Prelude.Eq Gender
 
instance Prelude.Ord Gender
 
instance Prelude.Show Gender
 
instance Data.EBF.EBF Gender
 
instance Data.Typeable.Typeable Gender
 
instance Data.EBF.TypeIdent Gender
 
instance Prelude.Enum Gender