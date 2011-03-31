{-# OPTIONS -XEmptyDataDecls #-}
{-# OPTIONS -XKindSignatures #-}
{-# OPTIONS -XNoImplicitPrelude #-}
{-# OPTIONS -XFlexibleContexts #-}
{-# OPTIONS -XUndecidableInstances #-}
{-# OPTIONS -XStandaloneDeriving #-}
module Typeable.T9f64aa567f1d4456b7cef6bf7f299c06 where
import Prelude
       (fromInteger, return, fail, undefined, (>>=), (>>), (==))
import qualified Prelude
import qualified Data.Binary
import qualified Data.Binary.Put
import qualified Data.Binary.Get
import qualified Data.EBF
import qualified Typeable.T4f7db06c439541658a09689d3e7dd909
import qualified Typeable.Ta9c059006c8d4849af902d3ad12ee3cc
 
data Host = IP{ip :: Typeable.Ta9c059006c8d4849af902d3ad12ee3cc.IP}
          | RegName{regName ::
                    Typeable.T4f7db06c439541658a09689d3e7dd909.Text}
 
deriving instance Prelude.Eq Host
 
deriving instance Prelude.Ord Host
 
deriving instance Prelude.Show Host
 
instance Data.EBF.EBF Host where
        get
          = do index <- Data.Binary.Get.getWord8
               case index of
                   0 -> (>>=) Data.EBF.get (\ a0 -> return (IP a0))
                   1 -> (>>=) Data.EBF.get (\ a0 -> return (RegName a0))
        put (IP a)
          = do Data.Binary.Put.putWord8 0
               Data.EBF.put a
        put (RegName a)
          = do Data.Binary.Put.putWord8 1
               Data.EBF.put a