{-# OPTIONS -XEmptyDataDecls #-}
{-# OPTIONS -XKindSignatures #-}
{-# OPTIONS -XNoImplicitPrelude #-}
{-# OPTIONS -XFlexibleContexts #-}
{-# OPTIONS -XUndecidableInstances #-}
{-# OPTIONS -XStandaloneDeriving #-}
module Typeable.T5448c6b79a084b4ea40d442c4fd2e125 where
import Prelude
       (fromInteger, return, fail, undefined, (>>=), (>>), (==))
import qualified Prelude
import qualified Data.Typeable
import qualified Data.Typeable.Extra
import qualified Data.Binary
import qualified Data.Binary.Put
import qualified Data.Binary.Get
import qualified Data.EBF
import qualified Typeable.T6ffbfb8682ad4f6a89d73e6d36c8fc7a
import qualified Typeable.Ta078d5123ead415d8d857dc6dc15b475
 
data Path = AbsolutePath{absolute ::
                         Typeable.T6ffbfb8682ad4f6a89d73e6d36c8fc7a.AbsolutePath}
          | RootlessPath{rootless ::
                         Typeable.Ta078d5123ead415d8d857dc6dc15b475.RootlessPath}
 
deriving instance Prelude.Eq Path
 
deriving instance Prelude.Ord Path
 
deriving instance Prelude.Show Path
 
instance Data.EBF.EBF Path where
        get
          = do index <- Data.Binary.Get.getWord8
               case index of
                   0 -> (>>=) Data.EBF.get (\ a0 -> return (AbsolutePath a0))
                   1 -> (>>=) Data.EBF.get (\ a0 -> return (RootlessPath a0))
        put (AbsolutePath a)
          = do Data.Binary.Put.putWord8 0
               Data.EBF.put a
        put (RootlessPath a)
          = do Data.Binary.Put.putWord8 1
               Data.EBF.put a
 
instance Data.Typeable.Typeable Path where
        typeOf _
          = Data.Typeable.mkTyConApp
              (Data.Typeable.mkTyCon
                 "Typeable.T5448c6b79a084b4ea40d442c4fd2e125")
              []