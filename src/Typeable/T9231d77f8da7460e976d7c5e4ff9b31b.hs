{-# OPTIONS -XEmptyDataDecls #-}
{-# OPTIONS -XKindSignatures #-}
{-# OPTIONS -XNoImplicitPrelude #-}
{-# OPTIONS -XFlexibleContexts #-}
{-# OPTIONS -XUndecidableInstances #-}
{-# OPTIONS -XStandaloneDeriving #-}
module Typeable.T9231d77f8da7460e976d7c5e4ff9b31b where
import Prelude
       (fromInteger, return, fail, undefined, (>>=), (>>), (==))
import qualified Prelude
import qualified Data.Binary
import qualified Data.Binary.Put
import qualified Data.Binary.Get
import qualified Data.EBF
 
data Pattern = MatchAll{}
             | Constructor{}
 
deriving instance Prelude.Eq Pattern
 
deriving instance Prelude.Ord Pattern
 
deriving instance Prelude.Show Pattern
 
instance Data.EBF.EBF Pattern where
        get
          = do index <- Data.Binary.Get.getWord8
               case index of
                   0 -> return MatchAll
                   1 -> return Constructor
        put MatchAll = do Data.Binary.Put.putWord8 0
        put Constructor = do Data.Binary.Put.putWord8 1
 
deriving instance Prelude.Enum Pattern