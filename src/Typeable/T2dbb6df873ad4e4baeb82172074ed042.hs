{-# OPTIONS -XEmptyDataDecls #-}
{-# OPTIONS -XKindSignatures #-}
{-# OPTIONS -XNoImplicitPrelude #-}
{-# OPTIONS -XFlexibleContexts #-}
{-# OPTIONS -XUndecidableInstances #-}
{-# OPTIONS -XStandaloneDeriving #-}
{-# OPTIONS -XOverloadedStrings #-}
{-# OPTIONS -XDeriveDataTypeable #-}
module Typeable.T2dbb6df873ad4e4baeb82172074ed042 where
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
 
data Gender = Male{}
            | Female{}
 
deriving instance Prelude.Eq Gender
 
deriving instance Prelude.Ord Gender
 
deriving instance Prelude.Show Gender
 
instance Data.EBF.EBF Gender where
        get
          = do index <- Data.Binary.Get.getWord8
               case index of
                   0 -> return Male
                   1 -> return Female
        put Male = do Data.Binary.Put.putWord8 0
        put Female = do Data.Binary.Put.putWord8 1
 
instance Data.Typeable.Typeable Gender where
        typeOf _
          = Data.Typeable.mkTyConApp
              (Data.Typeable.mkTyCon
                 "Typeable.T2dbb6df873ad4e4baeb82172074ed042.Gender")
              []
 
instance Data.EBF.TypeIdent Gender where
        typeOf _ = Data.Tree.Node "2dbb6df8-73ad-4e4b-aeb8-2172074ed042" []
 
deriving instance Prelude.Enum Gender