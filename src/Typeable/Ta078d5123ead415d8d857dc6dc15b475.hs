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
import qualified Data.Binary
import qualified Data.Binary.Put
import qualified Data.Binary.Get
import qualified Typeable.Internal.EBF
import qualified Typeable.T4f7db06c439541658a09689d3e7dd909
import qualified Typeable.T0ba85f3f10099c75d4b696d0cf944e09
 
data RootlessPath = RootlessPath{segments ::
                                 Typeable.T0ba85f3f10099c75d4b696d0cf944e09.List
                                   Typeable.T4f7db06c439541658a09689d3e7dd909.Text}
 
deriving instance Prelude.Eq RootlessPath
 
deriving instance Prelude.Ord RootlessPath
 
deriving instance Prelude.Show RootlessPath
 
instance Typeable.Internal.EBF.EBF RootlessPath where
        get
          = do index <- return 0
               case index of
                   0 -> (>>=) Typeable.Internal.EBF.get
                          (\ a0 -> return (RootlessPath a0))
        put (RootlessPath a) = do Typeable.Internal.EBF.put a