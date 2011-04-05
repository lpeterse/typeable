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
import qualified Data.Tree
import qualified Data.Typeable
import qualified Data.Typeable.Extra
import qualified Data.Binary
import qualified Data.Binary.Put
import qualified Data.Binary.Get
import qualified Data.EBF
import qualified Typeable.T346674042a7248b4a94abff0726d0c43 as UUID
 
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
                 "Typeable.Tf47867c11a4d4e30ab652240dd8e72ba.Void")
              []
 
instance Data.EBF.TypeIdent Void where
        typeOf _
          = Data.Tree.Node
              (UUID.UUID 324956810981908260464868397207580996282)
              []
 
instance Prelude.Enum Void where
        succ = undefined
        pred = undefined
        toEnum = undefined
        fromEnum = undefined