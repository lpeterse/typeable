{-# OPTIONS -XEmptyDataDecls #-}
{-# OPTIONS -XKindSignatures #-}
{-# OPTIONS -XNoImplicitPrelude #-}
{-# OPTIONS -XFlexibleContexts #-}
{-# OPTIONS -XUndecidableInstances #-}
{-# OPTIONS -XStandaloneDeriving #-}
module Typeable.Ta0bbed7211664a319e09dc1c0f97bbd6 where
import Prelude
       (fromInteger, return, fail, undefined, (>>=), (>>), (==))
import qualified Prelude
import qualified Data.Binary
import qualified Data.Binary.Put
import qualified Data.Binary.Get
import qualified Typeable.Internal.EBF
 
data Casus = Nominativus{}
           | Genetivus{}
           | Dativus{}
           | Accusativus{}
           | Ablativus{}
           | Vocativus{}
 
deriving instance Prelude.Eq Casus
 
deriving instance Prelude.Ord Casus
 
deriving instance Prelude.Show Casus
 
instance Typeable.Internal.EBF.EBF Casus where
        get
          = do index <- Data.Binary.Get.getWord8
               case index of
                   0 -> return Nominativus
                   1 -> return Genetivus
                   2 -> return Dativus
                   3 -> return Accusativus
                   4 -> return Ablativus
                   5 -> return Vocativus
        put Nominativus = do Data.Binary.Put.putWord8 0
        put Genetivus = do Data.Binary.Put.putWord8 1
        put Dativus = do Data.Binary.Put.putWord8 2
        put Accusativus = do Data.Binary.Put.putWord8 3
        put Ablativus = do Data.Binary.Put.putWord8 4
        put Vocativus = do Data.Binary.Put.putWord8 5
 
deriving instance Prelude.Enum Casus