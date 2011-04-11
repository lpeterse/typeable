{-# OPTIONS -XEmptyDataDecls #-}
{-# OPTIONS -XKindSignatures #-}
{-# OPTIONS -XNoImplicitPrelude #-}
{-# OPTIONS -XFlexibleContexts #-}
{-# OPTIONS -XUndecidableInstances #-}
{-# OPTIONS -XStandaloneDeriving #-}
{-# OPTIONS -XOverloadedStrings #-}
{-# OPTIONS -XDeriveDataTypeable #-}
module Typeable.T421496848904471ea3197f25e2a02b72 where
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
 
data Zero
 
instance Prelude.Eq Zero where
        (==) = undefined
 
instance Prelude.Ord Zero where
        compare = undefined
 
instance Prelude.Show Zero where
        show = undefined
 
instance Data.EBF.EBF Zero where
        get = undefined
        put = undefined
 
instance Data.Typeable.Typeable Zero where
        typeOf _
          = Data.Typeable.mkTyConApp
              (Data.Typeable.mkTyCon
                 "Typeable.T421496848904471ea3197f25e2a02b72.Zero")
              []
 
instance Data.EBF.TypeIdent Zero where
        typeOf _ = Data.Tree.Node "42149684-8904-471e-a319-7f25e2a02b72" []
 
instance Prelude.Enum Zero where
        succ = undefined
        pred = undefined
        toEnum = undefined
        fromEnum = undefined