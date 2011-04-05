{-# OPTIONS -XEmptyDataDecls #-}
{-# OPTIONS -XKindSignatures #-}
{-# OPTIONS -XNoImplicitPrelude #-}
{-# OPTIONS -XFlexibleContexts #-}
{-# OPTIONS -XUndecidableInstances #-}
{-# OPTIONS -XStandaloneDeriving #-}
module Typeable.T335b76330e724b64a5256190fb579dad where
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
import qualified Typeable.Tf8f49ef6bbe874a42926fa23d5b3bc19
import qualified Typeable.T4f7db06c439541658a09689d3e7dd909
import qualified Typeable.T62d2d5371f08461aa328bc06561594f6
import qualified Typeable.T9f64aa567f1d4456b7cef6bf7f299c06
 
data Authority = Authority{userinfo ::
                           Typeable.T4f7db06c439541658a09689d3e7dd909.Text,
                           host :: Typeable.T9f64aa567f1d4456b7cef6bf7f299c06.Host,
                           port ::
                           Typeable.Tf8f49ef6bbe874a42926fa23d5b3bc19.Maybe
                             Typeable.T62d2d5371f08461aa328bc06561594f6.Word}
 
deriving instance Prelude.Eq Authority
 
deriving instance Prelude.Ord Authority
 
deriving instance Prelude.Show Authority
 
instance Data.EBF.EBF Authority where
        get
          = do index <- return 0
               case index of
                   0 -> (>>=) Data.EBF.get
                          (\ a0 ->
                             (>>=) Data.EBF.get
                               (\ a1 -> (>>=) Data.EBF.get (\ a2 -> return (Authority a0 a1 a2))))
        put (Authority a b c)
          = do Data.EBF.put a
               Data.EBF.put b
               Data.EBF.put c
 
instance Data.Typeable.Typeable Authority where
        typeOf _
          = Data.Typeable.mkTyConApp
              (Data.Typeable.mkTyCon
                 "Typeable.T335b76330e724b64a5256190fb579dad.Authority")
              []
 
instance Data.EBF.TypeIdent Authority where
        typeOf _
          = Data.Tree.Node (UUID.UUID 68265524168597861671877224434775858605)
              []