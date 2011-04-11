{-# OPTIONS -XEmptyDataDecls #-}
{-# OPTIONS -XKindSignatures #-}
{-# OPTIONS -XNoImplicitPrelude #-}
{-# OPTIONS -XFlexibleContexts #-}
{-# OPTIONS -XUndecidableInstances #-}
{-# OPTIONS -XStandaloneDeriving #-}
{-# OPTIONS -XOverloadedStrings #-}
{-# OPTIONS -XDeriveDataTypeable #-}
module Typeable.Taaaa6ecd826d4604801c4fa962cc1446 where
import Prelude
       (fromInteger, return, fail, undefined, (>>=), (>>), (==))
import qualified Prelude
import qualified Data.Tree
import qualified Data.Data
import qualified Data.Typeable
import qualified Data.Typeable.Extra
import qualified Data.Binary
import qualified Data.Binary.Put
import qualified Data.Binary.Get
import qualified Data.EBF
import Data.String
 
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
        typeOf _ = Data.Tree.Node "aaaa6ecd-826d-4604-801c-4fa962cc1446" []
 
instance Prelude.Enum TAI where
        succ = undefined
        pred = undefined
        toEnum = undefined
        fromEnum = undefined