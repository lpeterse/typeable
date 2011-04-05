{-# OPTIONS -XEmptyDataDecls #-}
{-# OPTIONS -XKindSignatures #-}
{-# OPTIONS -XNoImplicitPrelude #-}
{-# OPTIONS -XFlexibleContexts #-}
{-# OPTIONS -XUndecidableInstances #-}
{-# OPTIONS -XStandaloneDeriving #-}
{-# OPTIONS -XOverloadedStrings #-}
module Typeable.T10f280df659654becb6e08122e846284 where
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
import Data.String
 
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
 
instance Data.Typeable.Typeable Unit where
        typeOf _
          = Data.Typeable.mkTyConApp
              (Data.Typeable.mkTyCon
                 "Typeable.T10f280df659654becb6e08122e846284.Unit")
              []
 
instance Data.EBF.TypeIdent Unit where
        typeOf _ = Data.Tree.Node "10f280df-6596-54be-cb6e-08122e846284" []
 
deriving instance Prelude.Enum Unit