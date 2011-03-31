{-# OPTIONS -XEmptyDataDecls #-}
{-# OPTIONS -XKindSignatures #-}
{-# OPTIONS -XNoImplicitPrelude #-}
{-# OPTIONS -XFlexibleContexts #-}
{-# OPTIONS -XUndecidableInstances #-}
{-# OPTIONS -XStandaloneDeriving #-}
module Typeable.T10f280df659654becb6e08122e846284 where
import Prelude
       (fromInteger, return, fail, undefined, (>>=), (>>), (==))
import qualified Prelude
import qualified Data.Binary
import qualified Data.Binary.Put
import qualified Data.Binary.Get
import qualified Data.EBF
 
data Unit = Unit{}
 
deriving instance Prelude.Eq Unit
 
deriving instance Prelude.Ord Unit
 
deriving instance Prelude.Show Unit
 
instance Data.EBF.EBF Unit where
        get
          = do index <- return 0
               case index of
                   0 -> return Unit
        put Unit = return ()
 
deriving instance Prelude.Enum Unit