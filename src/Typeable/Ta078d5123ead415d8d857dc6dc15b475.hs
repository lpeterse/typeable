{-# OPTIONS -XEmptyDataDecls #-}
{-# OPTIONS -XKindSignatures #-}
{-# OPTIONS -XNoImplicitPrelude #-}
{-# OPTIONS -XFlexibleContexts #-}
{-# OPTIONS -XUndecidableInstances #-}
{-# OPTIONS -XStandaloneDeriving #-}
{-# OPTIONS -XOverloadedStrings #-}
{-# OPTIONS -XDeriveDataTypeable #-}
module Typeable.Ta078d5123ead415d8d857dc6dc15b475 where
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
import qualified Typeable.T0ba85f3f10099c75d4b696d0cf944e09
import qualified Typeable.T4f7db06c439541658a09689d3e7dd909
 
data RootlessPath = RootlessPath{segments ::
                                 Typeable.T0ba85f3f10099c75d4b696d0cf944e09.List
                                   Typeable.T4f7db06c439541658a09689d3e7dd909.Text}
 
deriving instance Prelude.Eq RootlessPath
 
deriving instance Prelude.Ord RootlessPath
 
deriving instance Prelude.Show RootlessPath
 
instance Data.EBF.EBF RootlessPath where
        get
          = do index <- return 0
               case index of
                   0 -> (>>=) Data.EBF.get (\ a0 -> return (RootlessPath a0))
        put (RootlessPath a) = do Data.EBF.put a
 
instance Data.Typeable.Typeable RootlessPath where
        typeOf _
          = Data.Typeable.mkTyConApp
              (Data.Typeable.mkTyCon
                 "Typeable.Ta078d5123ead415d8d857dc6dc15b475.RootlessPath")
              []
 
instance Data.EBF.TypeIdent RootlessPath where
        typeOf _ = Data.Tree.Node "a078d512-3ead-415d-8d85-7dc6dc15b475" []