{-# OPTIONS -XEmptyDataDecls #-}
{-# OPTIONS -XKindSignatures #-}
{-# OPTIONS -XNoImplicitPrelude #-}
{-# OPTIONS -XFlexibleContexts #-}
{-# OPTIONS -XUndecidableInstances #-}
{-# OPTIONS -XStandaloneDeriving #-}
module Typeable.T606d6a2553714be6b0467d2eb2c0708e where
import Prelude
       (fromInteger, return, fail, undefined, (>>=), (>>), (==))
import qualified Prelude
import qualified Data.Typeable
import qualified Data.Typeable.Extra
import qualified Data.Binary
import qualified Data.Binary.Put
import qualified Data.Binary.Get
import qualified Data.EBF
 
data Concrete
 
instance Prelude.Eq Concrete
 
instance Prelude.Ord Concrete
 
instance Prelude.Show Concrete
 
instance Data.EBF.EBF Concrete
 
instance Data.Typeable.Typeable Concrete
 
instance Prelude.Enum Concrete