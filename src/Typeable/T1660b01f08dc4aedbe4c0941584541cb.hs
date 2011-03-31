{-# OPTIONS -XEmptyDataDecls #-}
{-# OPTIONS -XKindSignatures #-}
{-# OPTIONS -XNoImplicitPrelude #-}
{-# OPTIONS -XFlexibleContexts #-}
{-# OPTIONS -XUndecidableInstances #-}
{-# OPTIONS -XStandaloneDeriving #-}
module Typeable.T1660b01f08dc4aedbe4c0941584541cb where
import Prelude
       (fromInteger, return, fail, undefined, (>>=), (>>), (==))
import qualified Prelude
import qualified Data.Binary
import qualified Data.Binary.Put
import qualified Data.Binary.Get
import qualified Data.EBF
 
data Kind = KindStar{}
          | KindApplication{function ::
                            Typeable.T1660b01f08dc4aedbe4c0941584541cb.Kind,
                            argument :: Typeable.T1660b01f08dc4aedbe4c0941584541cb.Kind}
 
deriving instance Prelude.Eq Kind
 
deriving instance Prelude.Ord Kind
 
deriving instance Prelude.Show Kind
 
instance Data.EBF.EBF Kind where
        get
          = do index <- Data.Binary.Get.getWord8
               case index of
                   0 -> return KindStar
                   1 -> (>>=) Data.EBF.get
                          (\ a0 ->
                             (>>=) Data.EBF.get (\ a1 -> return (KindApplication a0 a1)))
        put KindStar = do Data.Binary.Put.putWord8 0
        put (KindApplication a b)
          = do Data.Binary.Put.putWord8 1
               Data.EBF.put a
               Data.EBF.put b