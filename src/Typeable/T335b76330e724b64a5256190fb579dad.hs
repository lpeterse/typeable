{-# OPTIONS -XEmptyDataDecls #-}
{-# OPTIONS -XKindSignatures #-}
{-# OPTIONS -XNoImplicitPrelude #-}
{-# OPTIONS -XFlexibleContexts #-}
{-# OPTIONS -XUndecidableInstances #-}
{-# OPTIONS -XStandaloneDeriving #-}
{-# OPTIONS -XOverloadedStrings #-}
{-# OPTIONS -XDeriveDataTypeable #-}
module Typeable.T335b76330e724b64a5256190fb579dad where
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
import qualified Typeable.T62d2d5371f08461aa328bc06561594f6
import qualified Typeable.T9f64aa567f1d4456b7cef6bf7f299c06
import qualified Typeable.Tf8f49ef6bbe874a42926fa23d5b3bc19
 
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
        typeOf _ = Data.Tree.Node "335b7633-0e72-4b64-a525-6190fb579dad" []