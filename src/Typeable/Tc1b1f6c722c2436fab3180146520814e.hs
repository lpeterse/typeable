{-# OPTIONS -XEmptyDataDecls #-}
{-# OPTIONS -XKindSignatures #-}
{-# OPTIONS -XNoImplicitPrelude #-}
{-# OPTIONS -XFlexibleContexts #-}
{-# OPTIONS -XUndecidableInstances #-}
{-# OPTIONS -XStandaloneDeriving #-}
module Typeable.Tc1b1f6c722c2436fab3180146520814e where
import Prelude
       (fromInteger, return, fail, undefined, (>>=), (>>), (==))
import qualified Prelude
import qualified Data.Binary
import qualified Data.Binary.Put
import qualified Data.Binary.Get
import qualified Data.EBF
 
data UTC
 
instance Prelude.Eq UTC where
        (==) = undefined
 
instance Prelude.Ord UTC where
        compare = undefined
 
instance Prelude.Show UTC where
        show = undefined
 
instance Data.EBF.EBF UTC where
        get = undefined
        put = undefined
 
instance Prelude.Enum UTC where
        succ = undefined
        pred = undefined
        toEnum = undefined
        fromEnum = undefined