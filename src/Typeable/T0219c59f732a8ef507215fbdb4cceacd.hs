{-# OPTIONS -XEmptyDataDecls #-}
{-# OPTIONS -XKindSignatures #-}
{-# OPTIONS -XNoImplicitPrelude #-}
{-# OPTIONS -XFlexibleContexts #-}
{-# OPTIONS -XUndecidableInstances #-}
{-# OPTIONS -XStandaloneDeriving #-}
module Typeable.T0219c59f732a8ef507215fbdb4cceacd where
import Prelude
       (fromInteger, return, fail, undefined, (>>=), (>>), (==))
import qualified Prelude
import qualified Data.Binary
import qualified Data.Binary.Put
import qualified Data.Binary.Get
import qualified Typeable.Internal.EBF
 
data Bool = False{}
          | True{}
 
deriving instance Prelude.Eq Bool
 
deriving instance Prelude.Ord Bool
 
deriving instance Prelude.Show Bool
 
instance Typeable.Internal.EBF.EBF Bool where
        get
          = do index <- Data.Binary.Get.getWord8
               case index of
                   0 -> return False
                   1 -> return True
        put False = do Data.Binary.Put.putWord8 0
        put True = do Data.Binary.Put.putWord8 1
 
deriving instance Prelude.Enum Bool