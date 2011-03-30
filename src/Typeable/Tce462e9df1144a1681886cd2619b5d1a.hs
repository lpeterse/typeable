{-# OPTIONS -XEmptyDataDecls #-}
{-# OPTIONS -XKindSignatures #-}
{-# OPTIONS -XNoImplicitPrelude #-}
{-# OPTIONS -XFlexibleContexts #-}
{-# OPTIONS -XUndecidableInstances #-}
{-# OPTIONS -XStandaloneDeriving #-}
module Typeable.Tce462e9df1144a1681886cd2619b5d1a where
import Prelude
       (fromInteger, return, fail, undefined, (>>=), (>>), (==))
import qualified Prelude
import qualified Data.Binary
import qualified Data.Binary.Put
import qualified Data.Binary.Get
import qualified Typeable.Internal.EBF
 
data Genus = Masculinum{}
           | Femininum{}
           | Neutrum{}
 
deriving instance Prelude.Eq Genus
 
deriving instance Prelude.Ord Genus
 
deriving instance Prelude.Show Genus
 
instance Typeable.Internal.EBF.EBF Genus where
        get
          = do index <- Data.Binary.Get.getWord8
               case index of
                   0 -> return Masculinum
                   1 -> return Femininum
                   2 -> return Neutrum
        put Masculinum = do Data.Binary.Put.putWord8 0
        put Femininum = do Data.Binary.Put.putWord8 1
        put Neutrum = do Data.Binary.Put.putWord8 2
 
deriving instance Prelude.Enum Genus