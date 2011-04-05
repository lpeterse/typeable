{-# OPTIONS -XEmptyDataDecls #-}
{-# OPTIONS -XKindSignatures #-}
{-# OPTIONS -XNoImplicitPrelude #-}
{-# OPTIONS -XFlexibleContexts #-}
{-# OPTIONS -XUndecidableInstances #-}
{-# OPTIONS -XStandaloneDeriving #-}
{-# OPTIONS -XOverloadedStrings #-}
module Typeable.Tce462e9df1144a1681886cd2619b5d1a where
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
 
data Genus = Masculinum{}
           | Femininum{}
           | Neutrum{}
 
deriving instance Prelude.Eq Genus
 
deriving instance Prelude.Ord Genus
 
deriving instance Prelude.Show Genus
 
instance Data.EBF.EBF Genus where
        get
          = do index <- Data.Binary.Get.getWord8
               case index of
                   0 -> return Masculinum
                   1 -> return Femininum
                   2 -> return Neutrum
        put Masculinum = do Data.Binary.Put.putWord8 0
        put Femininum = do Data.Binary.Put.putWord8 1
        put Neutrum = do Data.Binary.Put.putWord8 2
 
instance Data.Typeable.Typeable Genus where
        typeOf _
          = Data.Typeable.mkTyConApp
              (Data.Typeable.mkTyCon
                 "Typeable.Tce462e9df1144a1681886cd2619b5d1a.Genus")
              []
 
instance Data.EBF.TypeIdent Genus where
        typeOf _ = Data.Tree.Node "ce462e9d-f114-4a16-8188-6cd2619b5d1a" []
 
deriving instance Prelude.Enum Genus