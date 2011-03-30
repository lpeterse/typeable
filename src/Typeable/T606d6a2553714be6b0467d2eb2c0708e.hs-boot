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
import qualified Data.Binary
import qualified Data.Binary.Put
import qualified Data.Binary.Get
import qualified Typeable.Internal.EBF
 
data Concrete
 
instance Prelude.Eq Concrete
 
instance Prelude.Ord Concrete
 
instance Prelude.Show Concrete
 
instance Typeable.Internal.EBF.EBF Concrete
 
instance Prelude.Enum Concrete