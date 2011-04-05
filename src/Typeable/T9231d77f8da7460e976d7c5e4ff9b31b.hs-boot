{-# OPTIONS -XEmptyDataDecls #-}
{-# OPTIONS -XKindSignatures #-}
{-# OPTIONS -XNoImplicitPrelude #-}
{-# OPTIONS -XFlexibleContexts #-}
{-# OPTIONS -XUndecidableInstances #-}
{-# OPTIONS -XStandaloneDeriving #-}
module Typeable.T9231d77f8da7460e976d7c5e4ff9b31b where
import Prelude
       (fromInteger, return, fail, undefined, (>>=), (>>), (==))
import qualified Prelude
import qualified Data.Typeable
import qualified Data.Typeable.Extra
import qualified Data.Binary
import qualified Data.Binary.Put
import qualified Data.Binary.Get
import qualified Data.EBF
 
data Pattern
 
instance Prelude.Eq Pattern
 
instance Prelude.Ord Pattern
 
instance Prelude.Show Pattern
 
instance Data.EBF.EBF Pattern
 
instance Data.Typeable.Typeable Pattern
 
instance Prelude.Enum Pattern