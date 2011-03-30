{-# OPTIONS -XEmptyDataDecls #-}
{-# OPTIONS -XKindSignatures #-}
{-# OPTIONS -XNoImplicitPrelude #-}
{-# OPTIONS -XFlexibleContexts #-}
{-# OPTIONS -XUndecidableInstances #-}
{-# OPTIONS -XStandaloneDeriving #-}
module Typeable.Tf47867c11a4d4e30ab652240dd8e72ba where
import Prelude
       (fromInteger, return, fail, undefined, (>>=), (>>), (==))
import qualified Prelude
import qualified Data.Binary
import qualified Data.Binary.Put
import qualified Data.Binary.Get
import qualified Typeable.Internal.EBF
 
data Void
 
instance Prelude.Eq Void where
        (==) = undefined
 
instance Prelude.Ord Void where
        compare = undefined
 
instance Prelude.Show Void where
        show = undefined
 
instance Typeable.Internal.EBF.EBF Void where
        get = undefined
        put = undefined
 
instance Prelude.Enum Void where
        succ = undefined
        pred = undefined
        toEnum = undefined
        fromEnum = undefined