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
import qualified Data.EBF
 
data Concrete
 
instance Prelude.Eq Concrete where
        (==) = undefined
 
instance Prelude.Ord Concrete where
        compare = undefined
 
instance Prelude.Show Concrete where
        show = undefined
 
instance Data.EBF.EBF Concrete where
        get = undefined
        put = undefined
 
instance Prelude.Enum Concrete where
        succ = undefined
        pred = undefined
        toEnum = undefined
        fromEnum = undefined