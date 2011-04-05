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
import qualified Data.Tree
import qualified Data.Typeable
import qualified Data.Typeable.Extra
import qualified Data.Binary
import qualified Data.Binary.Put
import qualified Data.Binary.Get
import qualified Data.EBF
import qualified Typeable.T346674042a7248b4a94abff0726d0c43 as UUID
 
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
                 "Typeable.Ted098cc975df4cd0adb99a5b7dc48600.Mailto")
              []
 
instance Data.EBF.TypeIdent Mailto where
        typeOf _
          = Data.Tree.Node
              (UUID.UUID 315076621171436751284015363799296280064)
              []
 
instance Prelude.Enum Mailto where
        succ = undefined
        pred = undefined
        toEnum = undefined
        fromEnum = undefined