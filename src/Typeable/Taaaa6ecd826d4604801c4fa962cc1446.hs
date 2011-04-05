{-# OPTIONS -XEmptyDataDecls #-}
{-# OPTIONS -XKindSignatures #-}
{-# OPTIONS -XNoImplicitPrelude #-}
{-# OPTIONS -XFlexibleContexts #-}
{-# OPTIONS -XUndecidableInstances #-}
{-# OPTIONS -XStandaloneDeriving #-}
module Typeable.Taaaa6ecd826d4604801c4fa962cc1446 where
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
 
data TAI
 
instance Prelude.Eq TAI where
        (==) = undefined
 
instance Prelude.Ord TAI where
        compare = undefined
 
instance Prelude.Show TAI where
        show = undefined
 
instance Data.EBF.EBF TAI where
        get = undefined
        put = undefined
 
instance Data.Typeable.Typeable TAI where
        typeOf _
          = Data.Typeable.mkTyConApp
              (Data.Typeable.mkTyCon
                 "Typeable.Taaaa6ecd826d4604801c4fa962cc1446.TAI")
              []
 
instance Data.EBF.TypeIdent TAI where
        typeOf _
          = Data.Tree.Node
              (UUID.UUID 226853697096581491038218197107173233734)
              []
 
instance Prelude.Enum TAI where
        succ = undefined
        pred = undefined
        toEnum = undefined
        fromEnum = undefined