{-# OPTIONS -XEmptyDataDecls #-}
{-# OPTIONS -XKindSignatures #-}
{-# OPTIONS -XNoImplicitPrelude #-}
{-# OPTIONS -XFlexibleContexts #-}
{-# OPTIONS -XUndecidableInstances #-}
{-# OPTIONS -XStandaloneDeriving #-}
{-# OPTIONS -XOverloadedStrings #-}
{-# OPTIONS -XDeriveDataTypeable #-}
module Typeable.Tf4b6d72c609d4003ba98917f8c56a678 where
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
 
data Ordering = LT{}
              | EQ{}
              | GT{}
 
deriving instance Prelude.Eq Ordering
 
deriving instance Prelude.Ord Ordering
 
deriving instance Prelude.Show Ordering
 
instance Data.EBF.EBF Ordering where
        get
          = do index <- Data.Binary.Get.getWord8
               case index of
                   0 -> return LT
                   1 -> return EQ
                   2 -> return GT
        put LT = do Data.Binary.Put.putWord8 0
        put EQ = do Data.Binary.Put.putWord8 1
        put GT = do Data.Binary.Put.putWord8 2
 
instance Data.Typeable.Typeable Ordering where
        typeOf _
          = Data.Typeable.mkTyConApp
              (Data.Typeable.mkTyCon
                 "Typeable.Tf4b6d72c609d4003ba98917f8c56a678.Ordering")
              []
 
instance Data.EBF.TypeIdent Ordering where
        typeOf _ = Data.Tree.Node "f4b6d72c-609d-4003-ba98-917f8c56a678" []
 
deriving instance Prelude.Enum Ordering