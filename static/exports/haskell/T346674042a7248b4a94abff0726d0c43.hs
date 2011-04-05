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
import qualified Data.Typeable
import qualified Data.Typeable.Extra
import qualified Typeable.Tbbabbac1510d49aa9da25d8033147c54
 
data UUID = UUID{uuid ::
                 Typeable.Tbbabbac1510d49aa9da25d8033147c54.Word128}
 
deriving instance Prelude.Eq UUID
 
deriving instance Prelude.Ord UUID
 
deriving instance Prelude.Show UUID
 

instance Data.Typeable.Typeable UUID where
        typeOf _
          = Data.Typeable.mkTyConApp
              (Data.Typeable.mkTyCon
                 "Typeable.T346674042a7248b4a94abff0726d0c43.UUID")
              []
