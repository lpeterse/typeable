{-# OPTIONS -XEmptyDataDecls #-}
{-# OPTIONS -XKindSignatures #-}
{-# OPTIONS -XNoImplicitPrelude #-}
{-# OPTIONS -XFlexibleContexts #-}
{-# OPTIONS -XUndecidableInstances #-}
{-# OPTIONS -XStandaloneDeriving #-}
module Typeable.Tf4b6d72c609d4003ba98917f8c56a678 where
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
        typeOf _
          = Data.Tree.Node
              (UUID.UUID 325280993233777411950462891736228603512)
              []
 
deriving instance Prelude.Enum Ordering