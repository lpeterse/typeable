{-# OPTIONS -XEmptyDataDecls #-}
{-# OPTIONS -XKindSignatures #-}
{-# OPTIONS -XNoImplicitPrelude #-}
{-# OPTIONS -XFlexibleContexts #-}
{-# OPTIONS -XUndecidableInstances #-}
{-# OPTIONS -XStandaloneDeriving #-}
module Typeable.T1566edb1a4de4aab8106e63293e9bfcf where
import Prelude
       (fromInteger, return, fail, undefined, (>>=), (>>), (==))
import qualified Prelude
import qualified Data.Binary
import qualified Data.Binary.Put
import qualified Data.Binary.Get
import qualified Typeable.Internal.EBF
import qualified Typeable.T6716d098a58743379e54c12f249cdc0c
import qualified Typeable.Tff421b2c31774c37a7336c8245a74da9
 
data Symbol = Lower{lower ::
                    Typeable.T6716d098a58743379e54c12f249cdc0c.LatinAlphabet}
            | Upper{upper ::
                    Typeable.T6716d098a58743379e54c12f249cdc0c.LatinAlphabet}
            | Decimal{decimal ::
                      Typeable.Tff421b2c31774c37a7336c8245a74da9.DecimalAlphabet}
            | Underscore{}
            | Prime{}
 
deriving instance Prelude.Eq Symbol
 
deriving instance Prelude.Ord Symbol
 
deriving instance Prelude.Show Symbol
 
instance Typeable.Internal.EBF.EBF Symbol where
        get
          = do index <- Data.Binary.Get.getWord8
               case index of
                   0 -> (>>=) Typeable.Internal.EBF.get (\ a0 -> return (Lower a0))
                   1 -> (>>=) Typeable.Internal.EBF.get (\ a0 -> return (Upper a0))
                   2 -> (>>=) Typeable.Internal.EBF.get (\ a0 -> return (Decimal a0))
                   3 -> return Underscore
                   4 -> return Prime
        put (Lower a)
          = do Data.Binary.Put.putWord8 0
               Typeable.Internal.EBF.put a
        put (Upper a)
          = do Data.Binary.Put.putWord8 1
               Typeable.Internal.EBF.put a
        put (Decimal a)
          = do Data.Binary.Put.putWord8 2
               Typeable.Internal.EBF.put a
        put Underscore = do Data.Binary.Put.putWord8 3
        put Prime = do Data.Binary.Put.putWord8 4