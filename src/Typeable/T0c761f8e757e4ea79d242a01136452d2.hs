{-# OPTIONS -XEmptyDataDecls #-}
{-# OPTIONS -XKindSignatures #-}
{-# OPTIONS -XNoImplicitPrelude #-}
{-# OPTIONS -XFlexibleContexts #-}
{-# OPTIONS -XUndecidableInstances #-}
{-# OPTIONS -XStandaloneDeriving #-}
module Typeable.T0c761f8e757e4ea79d242a01136452d2 where
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
 
data SimpleMeta = SimpleMeta{name ::
                             Typeable.T4f7db06c439541658a09689d3e7dd909.Text,
                             comments :: Typeable.T4f7db06c439541658a09689d3e7dd909.Text}
 
deriving instance Prelude.Eq SimpleMeta
 
deriving instance Prelude.Ord SimpleMeta
 
deriving instance Prelude.Show SimpleMeta
 
instance Data.EBF.EBF SimpleMeta where
        get
          = do index <- return 0
               case index of
                   0 -> (>>=) Data.EBF.get
                          (\ a0 -> (>>=) Data.EBF.get (\ a1 -> return (SimpleMeta a0 a1)))
        put (SimpleMeta a b)
          = do Data.EBF.put a
               Data.EBF.put b
 
instance Data.Typeable.Typeable SimpleMeta where
        typeOf _
          = Data.Typeable.mkTyConApp
              (Data.Typeable.mkTyCon
                 "Typeable.T0c761f8e757e4ea79d242a01136452d2")
              []