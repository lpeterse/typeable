{-# OPTIONS -XEmptyDataDecls #-}
{-# OPTIONS -XKindSignatures #-}
{-# OPTIONS -XNoImplicitPrelude #-}
{-# OPTIONS -XFlexibleContexts #-}
{-# OPTIONS -XUndecidableInstances #-}
{-# OPTIONS -XStandaloneDeriving #-}
module Typeable.Ted098cc975df4cd0adb99a5b7dc48600 where
import Prelude
       (fromInteger, return, fail, undefined, (>>=), (>>), (==))
import qualified Prelude
import qualified Data.Typeable
import qualified Data.Typeable.Extra
import qualified Data.Binary
import qualified Data.Binary.Put
import qualified Data.Binary.Get
import qualified Data.EBF
 
data Mailto
 
instance Prelude.Eq Mailto where
        (==) = undefined
 
instance Prelude.Ord Mailto where
        compare = undefined
 
instance Prelude.Show Mailto where
        show = undefined
 
instance Data.EBF.EBF Mailto where
        get = undefined
        put = undefined
 
instance Data.Typeable.Typeable Mailto where
        typeOf _
          = Data.Typeable.mkTyConApp
              (Data.Typeable.mkTyCon
                 "Typeable.Ted098cc975df4cd0adb99a5b7dc48600")
              []
 
instance Prelude.Enum Mailto where
        succ = undefined
        pred = undefined
        toEnum = undefined
        fromEnum = undefined