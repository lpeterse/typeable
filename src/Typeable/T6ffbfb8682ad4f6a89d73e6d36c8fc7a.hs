{-# OPTIONS -XEmptyDataDecls #-}
{-# OPTIONS -XKindSignatures #-}
{-# OPTIONS -XNoImplicitPrelude #-}
{-# OPTIONS -XFlexibleContexts #-}
{-# OPTIONS -XUndecidableInstances #-}
{-# OPTIONS -XStandaloneDeriving #-}
module Typeable.T6ffbfb8682ad4f6a89d73e6d36c8fc7a where
import Prelude
       (fromInteger, return, fail, undefined, (>>=), (>>), (==))
import qualified Prelude
import qualified Data.Typeable
import qualified Data.Typeable.Extra
import qualified Data.Binary
import qualified Data.Binary.Put
import qualified Data.Binary.Get
import qualified Data.EBF
import qualified Typeable.T4f7db06c439541658a09689d3e7dd909
import qualified Typeable.T0ba85f3f10099c75d4b696d0cf944e09
 
data AbsolutePath = AbsolutePath{segments ::
                                 Typeable.T0ba85f3f10099c75d4b696d0cf944e09.List
                                   Typeable.T4f7db06c439541658a09689d3e7dd909.Text}
 
deriving instance Prelude.Eq AbsolutePath
 
deriving instance Prelude.Ord AbsolutePath
 
deriving instance Prelude.Show AbsolutePath
 
instance Data.EBF.EBF AbsolutePath where
        get
          = do index <- return 0
               case index of
                   0 -> (>>=) Data.EBF.get (\ a0 -> return (AbsolutePath a0))
        put (AbsolutePath a) = do Data.EBF.put a
 
instance Data.Typeable.Typeable AbsolutePath where
        typeOf _
          = Data.Typeable.mkTyConApp
              (Data.Typeable.mkTyCon
                 "Typeable.T6ffbfb8682ad4f6a89d73e6d36c8fc7a")
              []