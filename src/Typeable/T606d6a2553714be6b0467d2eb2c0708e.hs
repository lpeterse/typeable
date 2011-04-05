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
import qualified Data.Tree
import qualified Data.Typeable
import qualified Data.Typeable.Extra
import qualified Data.Binary
import qualified Data.Binary.Put
import qualified Data.Binary.Get
import qualified Data.EBF
import qualified Typeable.T346674042a7248b4a94abff0726d0c43 as UUID
 
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
 
instance Data.Typeable.Typeable Concrete where
        typeOf _
          = Data.Typeable.mkTyConApp
              (Data.Typeable.mkTyCon
                 "Typeable.T606d6a2553714be6b0467d2eb2c0708e.Concrete")
              []
 
instance Data.EBF.TypeIdent Concrete where
        typeOf _
          = Data.Tree.Node
              (UUID.UUID 128174000845616442980848154672613060750)
              []
 
instance Prelude.Enum Concrete where
        succ = undefined
        pred = undefined
        toEnum = undefined
        fromEnum = undefined