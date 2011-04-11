{-# OPTIONS -XEmptyDataDecls #-}
{-# OPTIONS -XKindSignatures #-}
{-# OPTIONS -XNoImplicitPrelude #-}
{-# OPTIONS -XFlexibleContexts #-}
{-# OPTIONS -XUndecidableInstances #-}
{-# OPTIONS -XStandaloneDeriving #-}
{-# OPTIONS -XOverloadedStrings #-}
{-# OPTIONS -XDeriveDataTypeable #-}
module Typeable.T26b9a53370bc4489a322192e2e0416ce where
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
import qualified Typeable.T2dbb6df873ad4e4baeb82172074ed042
import qualified Typeable.T4f7db06c439541658a09689d3e7dd909
 
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
 
instance Data.Typeable.Typeable SimpleSpeaker where
        typeOf _
          = Data.Typeable.mkTyConApp
              (Data.Typeable.mkTyCon
                 "Typeable.T26b9a53370bc4489a322192e2e0416ce.SimpleSpeaker")
              []
 
instance Data.EBF.TypeIdent SimpleSpeaker where
        typeOf _ = Data.Tree.Node "26b9a533-70bc-4489-a322-192e2e0416ce" []