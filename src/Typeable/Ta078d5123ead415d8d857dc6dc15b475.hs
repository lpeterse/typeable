{-# OPTIONS -XEmptyDataDecls #-}
{-# OPTIONS -XKindSignatures #-}
{-# OPTIONS -XNoImplicitPrelude #-}
{-# OPTIONS -XFlexibleContexts #-}
{-# OPTIONS -XUndecidableInstances #-}
{-# OPTIONS -XStandaloneDeriving #-}
module Typeable.Ta078d5123ead415d8d857dc6dc15b475 where
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
import qualified Typeable.T4f7db06c439541658a09689d3e7dd909
import qualified Typeable.T0ba85f3f10099c75d4b696d0cf944e09
 
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
        typeOf _
          = Data.Tree.Node
              (UUID.UUID 213303876547360745497874172367064118389)
              []