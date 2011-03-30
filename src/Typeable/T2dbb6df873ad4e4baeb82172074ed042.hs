{-# OPTIONS -XEmptyDataDecls #-}
{-# OPTIONS -XKindSignatures #-}
{-# OPTIONS -XNoImplicitPrelude #-}
{-# OPTIONS -XFlexibleContexts #-}
{-# OPTIONS -XUndecidableInstances #-}
{-# OPTIONS -XStandaloneDeriving #-}
module Typeable.T2dbb6df873ad4e4baeb82172074ed042 where
import Prelude
       (fromInteger, return, fail, undefined, (>>=), (>>), (==))
import qualified Prelude
import qualified Data.Binary
import qualified Data.Binary.Put
import qualified Data.Binary.Get
import qualified Typeable.Internal.EBF
 
data Gender = Male{}
            | Female{}
 
deriving instance Prelude.Eq Gender
 
deriving instance Prelude.Ord Gender
 
deriving instance Prelude.Show Gender
 
instance Typeable.Internal.EBF.EBF Gender where
        get
          = do index <- Data.Binary.Get.getWord8
               case index of
                   0 -> return Male
                   1 -> return Female
        put Male = do Data.Binary.Put.putWord8 0
        put Female = do Data.Binary.Put.putWord8 1
 
deriving instance Prelude.Enum Gender