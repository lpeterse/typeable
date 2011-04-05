{-# OPTIONS -XEmptyDataDecls #-}
{-# OPTIONS -XKindSignatures #-}
{-# OPTIONS -XNoImplicitPrelude #-}
{-# OPTIONS -XFlexibleContexts #-}
{-# OPTIONS -XUndecidableInstances #-}
{-# OPTIONS -XStandaloneDeriving #-}
module Typeable.T205895c8d2df475b8d5ead5ee33d9f63 where
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
import qualified Typeable.T2c62454c586f4bdea5e2b17e432db245
import qualified Typeable.T9790ade9814a4aaca5eaa80c3e47685d
import qualified Typeable.Tb0221a43509e4eddb062101bfd794bc4
import qualified Typeable.T0174bd2264004820bfe34e211cb35a7d
 
data Field (a :: *) = Field{name ::
                            Typeable.T9790ade9814a4aaca5eaa80c3e47685d.Designator,
                            semantics ::
                            Typeable.Tb0221a43509e4eddb062101bfd794bc4.StructuredText
                              (Typeable.T2c62454c586f4bdea5e2b17e432db245.Extension a),
                            type_ :: Typeable.T0174bd2264004820bfe34e211cb35a7d.DataType a}
 
deriving instance (Prelude.Eq a) => Prelude.Eq (Field a)
 
deriving instance (Prelude.Ord a) => Prelude.Ord (Field a)
 
deriving instance (Prelude.Show a) => Prelude.Show (Field a)
 
instance (Data.EBF.EBF a) => Data.EBF.EBF (Field a) where
        get
          = do index <- return 0
               case index of
                   0 -> (>>=) Data.EBF.get
                          (\ a0 ->
                             (>>=) Data.EBF.get
                               (\ a1 -> (>>=) Data.EBF.get (\ a2 -> return (Field a0 a1 a2))))
        put (Field a b c)
          = do Data.EBF.put a
               Data.EBF.put b
               Data.EBF.put c
 
instance Data.Typeable.Typeable1 Field where
        typeOf1 _
          = Data.Typeable.mkTyConApp
              (Data.Typeable.mkTyCon
                 "Typeable.T205895c8d2df475b8d5ead5ee33d9f63.Field")
              []
 
instance Data.EBF.TypeIdentS Field where
        typeOfS _
          = Data.Tree.Node (UUID.UUID 42995255978593599213110487675926585187)
              []