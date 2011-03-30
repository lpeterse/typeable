{-# OPTIONS -XEmptyDataDecls #-}
{-# OPTIONS -XKindSignatures #-}
{-# OPTIONS -XNoImplicitPrelude #-}
{-# OPTIONS -XFlexibleContexts #-}
{-# OPTIONS -XUndecidableInstances #-}
{-# OPTIONS -XStandaloneDeriving #-}
module Typeable.T2c62454c586f4bdea5e2b17e432db245 where
import Prelude
       (fromInteger, return, fail, undefined, (>>=), (>>), (==))
import qualified Prelude
import qualified Data.Binary
import qualified Data.Binary.Put
import qualified Data.Binary.Get
import qualified Typeable.Internal.EBF
 
data Extension (a :: *) = Type{}
                        | TypeConstructor{}
                        | TypeConstructorField{}
                        | Class{}
                        | ClassMethod{}
                        | Function{}
                        | Constraint{}
                        | Expression{}
 
deriving instance (Prelude.Eq a) => Prelude.Eq (Extension a)
 
deriving instance (Prelude.Ord a) => Prelude.Ord (Extension a)
 
deriving instance (Prelude.Show a) => Prelude.Show (Extension a)
 
instance (Typeable.Internal.EBF.EBF a) => Typeable.Internal.EBF.EBF
         (Extension a) where
        get
          = do index <- Data.Binary.Get.getWord8
               case index of
                   0 -> return Type
                   1 -> return TypeConstructor
                   2 -> return TypeConstructorField
                   3 -> return Class
                   4 -> return ClassMethod
                   5 -> return Function
                   6 -> return Constraint
                   7 -> return Expression
        put Type = do Data.Binary.Put.putWord8 0
        put TypeConstructor = do Data.Binary.Put.putWord8 1
        put TypeConstructorField = do Data.Binary.Put.putWord8 2
        put Class = do Data.Binary.Put.putWord8 3
        put ClassMethod = do Data.Binary.Put.putWord8 4
        put Function = do Data.Binary.Put.putWord8 5
        put Constraint = do Data.Binary.Put.putWord8 6
        put Expression = do Data.Binary.Put.putWord8 7