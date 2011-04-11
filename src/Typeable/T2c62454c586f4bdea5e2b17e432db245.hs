{-# OPTIONS -XEmptyDataDecls #-}
{-# OPTIONS -XKindSignatures #-}
{-# OPTIONS -XNoImplicitPrelude #-}
{-# OPTIONS -XFlexibleContexts #-}
{-# OPTIONS -XUndecidableInstances #-}
{-# OPTIONS -XStandaloneDeriving #-}
{-# OPTIONS -XOverloadedStrings #-}
{-# OPTIONS -XDeriveDataTypeable #-}
module Typeable.T2c62454c586f4bdea5e2b17e432db245 where
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
import qualified Typeable.T0174bd2264004820bfe34e211cb35a7d
import qualified Typeable.T346674042a7248b4a94abff0726d0c43
import qualified Typeable.T62d2d5371f08461aa328bc06561594f6
 
data Extension (a :: *) = Type{type_ ::
                               Typeable.T0174bd2264004820bfe34e211cb35a7d.DataType a}
                        | ValueConstructor{reference ::
                                           Typeable.T346674042a7248b4a94abff0726d0c43.UUID,
                                           constructorIndex ::
                                           Typeable.T62d2d5371f08461aa328bc06561594f6.Word}
                        | ValueConstructorField{reference ::
                                                Typeable.T346674042a7248b4a94abff0726d0c43.UUID,
                                                constructorIndex ::
                                                Typeable.T62d2d5371f08461aa328bc06561594f6.Word,
                                                fieldIndex ::
                                                Typeable.T62d2d5371f08461aa328bc06561594f6.Word}
                        | Class{}
                        | ClassMethod{}
                        | Constraint{}
                        | Expression{}
 
deriving instance (Prelude.Eq a) => Prelude.Eq (Extension a)
 
deriving instance (Prelude.Ord a) => Prelude.Ord (Extension a)
 
deriving instance (Prelude.Show a) => Prelude.Show (Extension a)
 
instance (Data.EBF.EBF a, Data.EBF.TypeIdent a) => Data.EBF.EBF
         (Extension a) where
        get
          = do index <- Data.Binary.Get.getWord8
               case index of
                   0 -> (>>=) Data.EBF.get (\ a0 -> return (Type a0))
                   1 -> (>>=) Data.EBF.get
                          (\ a0 ->
                             (>>=) Data.EBF.get (\ a1 -> return (ValueConstructor a0 a1)))
                   2 -> (>>=) Data.EBF.get
                          (\ a0 ->
                             (>>=) Data.EBF.get
                               (\ a1 ->
                                  (>>=) Data.EBF.get
                                    (\ a2 -> return (ValueConstructorField a0 a1 a2))))
                   3 -> return Class
                   4 -> return ClassMethod
                   5 -> return Constraint
                   6 -> return Expression
        put (Type a)
          = do Data.Binary.Put.putWord8 0
               Data.EBF.put a
        put (ValueConstructor a b)
          = do Data.Binary.Put.putWord8 1
               Data.EBF.put a
               Data.EBF.put b
        put (ValueConstructorField a b c)
          = do Data.Binary.Put.putWord8 2
               Data.EBF.put a
               Data.EBF.put b
               Data.EBF.put c
        put Class = do Data.Binary.Put.putWord8 3
        put ClassMethod = do Data.Binary.Put.putWord8 4
        put Constraint = do Data.Binary.Put.putWord8 5
        put Expression = do Data.Binary.Put.putWord8 6
 
instance Data.Typeable.Typeable1 Extension where
        typeOf1 _
          = Data.Typeable.mkTyConApp
              (Data.Typeable.mkTyCon
                 "Typeable.T2c62454c586f4bdea5e2b17e432db245.Extension")
              []
 
instance Data.EBF.TypeIdentS Extension where
        typeOfS _
          = Data.Tree.Node "2c62454c-586f-4bde-a5e2-b17e432db245" []