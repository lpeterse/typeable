{-# OPTIONS -XEmptyDataDecls #-}
{-# OPTIONS -XKindSignatures #-}
{-# OPTIONS -XNoImplicitPrelude #-}
{-# OPTIONS -XFlexibleContexts #-}
{-# OPTIONS -XUndecidableInstances #-}
{-# OPTIONS -XStandaloneDeriving #-}
module Typeable.T26b9a53370bc4489a322192e2e0416ce where
import Prelude
       (fromInteger, return, fail, undefined, (>>=), (>>), (==))
import qualified Prelude
import qualified Data.Binary
import qualified Data.Binary.Put
import qualified Data.Binary.Get
import qualified Data.EBF
import qualified Typeable.T4f7db06c439541658a09689d3e7dd909
import qualified Typeable.T2dbb6df873ad4e4baeb82172074ed042
 
data SimpleSpeaker = SimpleMeta{name ::
                                Typeable.T4f7db06c439541658a09689d3e7dd909.Text,
                                gender :: Typeable.T2dbb6df873ad4e4baeb82172074ed042.Gender}
 
deriving instance Prelude.Eq SimpleSpeaker
 
deriving instance Prelude.Ord SimpleSpeaker
 
deriving instance Prelude.Show SimpleSpeaker
 
instance Data.EBF.EBF SimpleSpeaker where
        get
          = do index <- return 0
               case index of
                   0 -> (>>=) Data.EBF.get
                          (\ a0 -> (>>=) Data.EBF.get (\ a1 -> return (SimpleMeta a0 a1)))
        put (SimpleMeta a b)
          = do Data.EBF.put a
               Data.EBF.put b