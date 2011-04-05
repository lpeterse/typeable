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
import qualified Data.Typeable
import qualified Data.Typeable.Extra
import qualified Data.Binary
import qualified Data.Binary.Put
import qualified Data.Binary.Get
import qualified Data.EBF
 
data Void
 
instance Prelude.Eq Void where
        (==) = undefined
 
instance Prelude.Ord Void where
        compare = undefined
 
instance Prelude.Show Void where
        show = undefined
 
instance Data.EBF.EBF Void where
        get = undefined
        put = undefined
 
instance Data.Typeable.Typeable Void where
        typeOf _
          = Data.Typeable.mkTyConApp
              (Data.Typeable.mkTyCon
                 "Typeable.Tf47867c11a4d4e30ab652240dd8e72ba")
              []
 
instance Prelude.Enum Void where
        succ = undefined
        pred = undefined
        toEnum = undefined
        fromEnum = undefined