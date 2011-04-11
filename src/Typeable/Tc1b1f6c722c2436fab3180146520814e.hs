{-# OPTIONS -XEmptyDataDecls #-}
{-# OPTIONS -XKindSignatures #-}
{-# OPTIONS -XNoImplicitPrelude #-}
{-# OPTIONS -XFlexibleContexts #-}
{-# OPTIONS -XUndecidableInstances #-}
{-# OPTIONS -XStandaloneDeriving #-}
{-# OPTIONS -XOverloadedStrings #-}
{-# OPTIONS -XDeriveDataTypeable #-}
module Typeable.Tc1b1f6c722c2436fab3180146520814e where
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
        typeOf _ = Data.Tree.Node "c1b1f6c7-22c2-436f-ab31-80146520814e" []
 
instance Prelude.Enum UTC where
        succ = undefined
        pred = undefined
        toEnum = undefined
        fromEnum = undefined