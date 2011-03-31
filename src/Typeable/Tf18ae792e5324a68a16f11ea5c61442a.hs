{-# OPTIONS -XEmptyDataDecls #-}
{-# OPTIONS -XKindSignatures #-}
{-# OPTIONS -XNoImplicitPrelude #-}
{-# OPTIONS -XFlexibleContexts #-}
{-# OPTIONS -XUndecidableInstances #-}
{-# OPTIONS -XStandaloneDeriving #-}
module Typeable.Tf18ae792e5324a68a16f11ea5c61442a where
import Prelude
       (fromInteger, return, fail, undefined, (>>=), (>>), (==))
import qualified Prelude
import qualified Data.Binary
import qualified Data.Binary.Put
import qualified Data.Binary.Get
import qualified Data.EBF
 
data Tel
 
instance Prelude.Eq Tel where
        (==) = undefined
 
instance Prelude.Ord Tel where
        compare = undefined
 
instance Prelude.Show Tel where
        show = undefined
 
instance Data.EBF.EBF Tel where
        get = undefined
        put = undefined
 
instance Prelude.Enum Tel where
        succ = undefined
        pred = undefined
        toEnum = undefined
        fromEnum = undefined