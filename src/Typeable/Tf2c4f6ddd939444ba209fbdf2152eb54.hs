{-# OPTIONS -XEmptyDataDecls #-}
{-# OPTIONS -XKindSignatures #-}
{-# OPTIONS -XNoImplicitPrelude #-}
{-# OPTIONS -XFlexibleContexts #-}
{-# OPTIONS -XUndecidableInstances #-}
{-# OPTIONS -XStandaloneDeriving #-}
module Typeable.Tf2c4f6ddd939444ba209fbdf2152eb54 where
import Prelude
       (fromInteger, return, fail, undefined, (>>=), (>>), (==))
import qualified Prelude
import qualified Data.Binary
import qualified Data.Binary.Put
import qualified Data.Binary.Get
import qualified Typeable.Internal.EBF
import qualified Typeable.T6716d098a58743379e54c12f249cdc0c
import qualified Typeable.Tff421b2c31774c37a7336c8245a74da9
 
data SchemeSymbol = Latin{latin ::
                          Typeable.T6716d098a58743379e54c12f249cdc0c.LatinAlphabet}
                  | Decimal{decimal ::
                            Typeable.Tff421b2c31774c37a7336c8245a74da9.DecimalAlphabet}
                  | Plus{}
                  | Minus{}
                  | FullStop{}
 
deriving instance Prelude.Eq SchemeSymbol
 
deriving instance Prelude.Ord SchemeSymbol
 
deriving instance Prelude.Show SchemeSymbol
 
instance Typeable.Internal.EBF.EBF SchemeSymbol where
        get
          = do index <- Data.Binary.Get.getWord8
               case index of
                   0 -> (>>=) Typeable.Internal.EBF.get (\ a0 -> return (Latin a0))
                   1 -> (>>=) Typeable.Internal.EBF.get (\ a0 -> return (Decimal a0))
                   2 -> return Plus
                   3 -> return Minus
                   4 -> return FullStop
        put (Latin a)
          = do Data.Binary.Put.putWord8 0
               Typeable.Internal.EBF.put a
        put (Decimal a)
          = do Data.Binary.Put.putWord8 1
               Typeable.Internal.EBF.put a
        put Plus = do Data.Binary.Put.putWord8 2
        put Minus = do Data.Binary.Put.putWord8 3
        put FullStop = do Data.Binary.Put.putWord8 4