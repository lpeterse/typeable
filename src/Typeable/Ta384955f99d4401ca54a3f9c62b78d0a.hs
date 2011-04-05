{-# OPTIONS -XEmptyDataDecls #-}
{-# OPTIONS -XKindSignatures #-}
{-# OPTIONS -XNoImplicitPrelude #-}
{-# OPTIONS -XFlexibleContexts #-}
{-# OPTIONS -XUndecidableInstances #-}
{-# OPTIONS -XStandaloneDeriving #-}
{-# OPTIONS -XOverloadedStrings #-}
module Typeable.Ta384955f99d4401ca54a3f9c62b78d0a where
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
 
data Numerus = Singularis{}
             | Pluralis{}
 
deriving instance Prelude.Eq Numerus
 
deriving instance Prelude.Ord Numerus
 
deriving instance Prelude.Show Numerus
 
instance Data.EBF.EBF Numerus where
        get
          = do index <- Data.Binary.Get.getWord8
               case index of
                   0 -> return Singularis
                   1 -> return Pluralis
        put Singularis = do Data.Binary.Put.putWord8 0
        put Pluralis = do Data.Binary.Put.putWord8 1
 
instance Data.Typeable.Typeable Numerus where
        typeOf _
          = Data.Typeable.mkTyConApp
              (Data.Typeable.mkTyCon
                 "Typeable.Ta384955f99d4401ca54a3f9c62b78d0a.Numerus")
              []
 
instance Data.EBF.TypeIdent Numerus where
        typeOf _ = Data.Tree.Node "a384955f-99d4-401c-a54a-3f9c62b78d0a" []
 
deriving instance Prelude.Enum Numerus