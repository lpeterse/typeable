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
import qualified Data.Binary
import qualified Data.Binary.Put
import qualified Data.Binary.Get
import qualified Typeable.Internal.EBF
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
 
instance Typeable.Internal.EBF.EBF Authority where
        get
          = do index <- return 0
               case index of
                   0 -> (>>=) Typeable.Internal.EBF.get
                          (\ a0 ->
                             (>>=) Typeable.Internal.EBF.get
                               (\ a1 ->
                                  (>>=) Typeable.Internal.EBF.get
                                    (\ a2 -> return (Authority a0 a1 a2))))
        put (Authority a b c)
          = do Typeable.Internal.EBF.put a
               Typeable.Internal.EBF.put b
               Typeable.Internal.EBF.put c