{-# OPTIONS -XEmptyDataDecls #-}
{-# OPTIONS -XKindSignatures #-}
{-# OPTIONS -XNoImplicitPrelude #-}
{-# OPTIONS -XFlexibleContexts #-}
{-# OPTIONS -XUndecidableInstances #-}
{-# OPTIONS -XStandaloneDeriving #-}
{-# OPTIONS -XOverloadedStrings #-}
{-# OPTIONS -XDeriveDataTypeable #-}
module Typeable.T3819884685d34bf19b3469304e15983d where
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
import qualified Typeable.T4f7db06c439541658a09689d3e7dd909
import qualified Typeable.T53e0d483a64144259dce752799d64305
import qualified Typeable.T7af30cce93724981a16a80f3f193dc33
 
data Person = Person{name ::
                     Typeable.T4f7db06c439541658a09689d3e7dd909.Text,
                     contacts ::
                     Typeable.T7af30cce93724981a16a80f3f193dc33.Set
                       Typeable.T53e0d483a64144259dce752799d64305.ContactInformation}
 
deriving instance Prelude.Eq Person
 
deriving instance Prelude.Ord Person
 
deriving instance Prelude.Show Person
 
instance Data.EBF.EBF Person where
        get
          = do index <- return 0
               case index of
                   0 -> (>>=) Data.EBF.get
                          (\ a0 -> (>>=) Data.EBF.get (\ a1 -> return (Person a0 a1)))
        put (Person a b)
          = do Data.EBF.put a
               Data.EBF.put b
 
instance Data.Typeable.Typeable Person where
        typeOf _
          = Data.Typeable.mkTyConApp
              (Data.Typeable.mkTyCon
                 "Typeable.T3819884685d34bf19b3469304e15983d.Person")
              []
 
instance Data.EBF.TypeIdent Person where
        typeOf _ = Data.Tree.Node "38198846-85d3-4bf1-9b34-69304e15983d" []