{-# OPTIONS -XEmptyDataDecls #-}
{-# OPTIONS -XKindSignatures #-}
{-# OPTIONS -XNoImplicitPrelude #-}
{-# OPTIONS -XFlexibleContexts #-}
{-# OPTIONS -XUndecidableInstances #-}
{-# OPTIONS -XStandaloneDeriving #-}
module Typeable.T346674042a7248b4a94abff0726d0c43 where
import Prelude
       (fromInteger, return, fail, undefined, (>>=), (>>), (==))
import qualified Prelude
import qualified Data.Binary
import qualified Data.Binary.Put
import qualified Data.Binary.Get
import qualified Data.EBF
import qualified Typeable.Tbbabbac1510d49aa9da25d8033147c54
 
data UUID = UUID{uuid ::
                 Typeable.Tbbabbac1510d49aa9da25d8033147c54.Word128}
 
deriving instance Prelude.Eq UUID
 
deriving instance Prelude.Ord UUID
 
deriving instance Prelude.Show UUID
 
instance Data.EBF.EBF UUID where
        get
          = do index <- return 0
               case index of
                   0 -> (>>=) Data.EBF.get (\ a0 -> return (UUID a0))
        put (UUID a) = do Data.EBF.put a