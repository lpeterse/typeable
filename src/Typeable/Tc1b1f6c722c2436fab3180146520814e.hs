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
import qualified Data.Tree
import qualified Data.Typeable
import qualified Data.Typeable.Extra
import qualified Data.Binary
import qualified Data.Binary.Put
import qualified Data.Binary.Get
import qualified Data.EBF
import qualified Typeable.T346674042a7248b4a94abff0726d0c43 as UUID
 
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
 
instance Data.Typeable.Typeable UTC where
        typeOf _
          = Data.Typeable.mkTyConApp
              (Data.Typeable.mkTyCon
                 "Typeable.Tc1b1f6c722c2436fab3180146520814e.UTC")
              []
 
instance Data.EBF.TypeIdent UTC where
        typeOf _
          = Data.Tree.Node
              (UUID.UUID 257465044980373607011621829147394867534)
              []
 
instance Prelude.Enum UTC where
        succ = undefined
        pred = undefined
        toEnum = undefined
        fromEnum = undefined