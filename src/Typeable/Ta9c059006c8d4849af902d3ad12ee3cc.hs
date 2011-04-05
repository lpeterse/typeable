{-# OPTIONS -XEmptyDataDecls #-}
{-# OPTIONS -XKindSignatures #-}
{-# OPTIONS -XNoImplicitPrelude #-}
{-# OPTIONS -XFlexibleContexts #-}
{-# OPTIONS -XUndecidableInstances #-}
{-# OPTIONS -XStandaloneDeriving #-}
module Typeable.Ta9c059006c8d4849af902d3ad12ee3cc where
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
import qualified Typeable.Tbbabbac1510d49aa9da25d8033147c54
import qualified Typeable.T1a55145e5bd21e8adc14067707192552
 
data IP = IPv6{ipv6 ::
               Typeable.Tbbabbac1510d49aa9da25d8033147c54.Word128}
        | IPv4{ipv4 :: Typeable.T1a55145e5bd21e8adc14067707192552.Word32}
 
deriving instance Prelude.Eq IP
 
deriving instance Prelude.Ord IP
 
deriving instance Prelude.Show IP
 
instance Data.EBF.EBF IP where
        get
          = do index <- Data.Binary.Get.getWord8
               case index of
                   0 -> (>>=) Data.EBF.get (\ a0 -> return (IPv6 a0))
                   1 -> (>>=) Data.EBF.get (\ a0 -> return (IPv4 a0))
        put (IPv6 a)
          = do Data.Binary.Put.putWord8 0
               Data.EBF.put a
        put (IPv4 a)
          = do Data.Binary.Put.putWord8 1
               Data.EBF.put a
 
instance Data.Typeable.Typeable IP where
        typeOf _
          = Data.Typeable.mkTyConApp
              (Data.Typeable.mkTyCon
                 "Typeable.Ta9c059006c8d4849af902d3ad12ee3cc.IP")
              []
 
instance Data.EBF.TypeIdent IP where
        typeOf _
          = Data.Tree.Node
              (UUID.UUID 225638257452539375395009655003040113612)
              []