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
import qualified Data.Tree
import qualified Data.Typeable
import qualified Data.Typeable.Extra
import qualified Data.Binary
import qualified Data.Binary.Put
import qualified Data.Binary.Get
import qualified Data.EBF
import qualified Typeable.T346674042a7248b4a94abff0726d0c43 as UUID
 
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
 
instance Data.Typeable.Typeable Tel where
        typeOf _
          = Data.Typeable.mkTyConApp
              (Data.Typeable.mkTyCon
                 "Typeable.Tf18ae792e5324a68a16f11ea5c61442a.Tel")
              []
 
instance Data.EBF.TypeIdent Tel where
        typeOf _
          = Data.Tree.Node
              (UUID.UUID 321065180825505567126298334770499765290)
              []
 
instance Prelude.Enum Tel where
        succ = undefined
        pred = undefined
        toEnum = undefined
        fromEnum = undefined