{-# OPTIONS -XEmptyDataDecls #-}
{-# OPTIONS -XKindSignatures #-}
{-# OPTIONS -XNoImplicitPrelude #-}
{-# OPTIONS -XFlexibleContexts #-}
{-# OPTIONS -XUndecidableInstances #-}
{-# OPTIONS -XStandaloneDeriving #-}
{-# OPTIONS -XOverloadedStrings #-}
{-# OPTIONS -XDeriveDataTypeable #-}
module Typeable.T8068cbdaf35e4618a7e798c67ff9bee0 where
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
import qualified Typeable.T335b76330e724b64a5256190fb579dad
import qualified Typeable.T6ffbfb8682ad4f6a89d73e6d36c8fc7a
import qualified Typeable.Ta078d5123ead415d8d857dc6dc15b475
 
data Hierarchy = Authority{authority ::
                           Typeable.T335b76330e724b64a5256190fb579dad.Authority,
                           absolute ::
                           Typeable.T6ffbfb8682ad4f6a89d73e6d36c8fc7a.AbsolutePath}
               | Path{rootless ::
                      Typeable.Ta078d5123ead415d8d857dc6dc15b475.RootlessPath}
 
deriving instance Prelude.Eq Hierarchy
 
deriving instance Prelude.Ord Hierarchy
 
deriving instance Prelude.Show Hierarchy
 
instance Data.EBF.EBF Hierarchy where
        get
          = do index <- Data.Binary.Get.getWord8
               case index of
                   0 -> (>>=) Data.EBF.get
                          (\ a0 -> (>>=) Data.EBF.get (\ a1 -> return (Authority a0 a1)))
                   1 -> (>>=) Data.EBF.get (\ a0 -> return (Path a0))
        put (Authority a b)
          = do Data.Binary.Put.putWord8 0
               Data.EBF.put a
               Data.EBF.put b
        put (Path a)
          = do Data.Binary.Put.putWord8 1
               Data.EBF.put a
 
instance Data.Typeable.Typeable Hierarchy where
        typeOf _
          = Data.Typeable.mkTyConApp
              (Data.Typeable.mkTyCon
                 "Typeable.T8068cbdaf35e4618a7e798c67ff9bee0.Hierarchy")
              []
 
instance Data.EBF.TypeIdent Hierarchy where
        typeOf _ = Data.Tree.Node "8068cbda-f35e-4618-a7e7-98c67ff9bee0" []