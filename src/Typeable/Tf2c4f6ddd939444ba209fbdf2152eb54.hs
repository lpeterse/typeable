{-# OPTIONS -XEmptyDataDecls #-}
{-# OPTIONS -XKindSignatures #-}
{-# OPTIONS -XNoImplicitPrelude #-}
{-# OPTIONS -XFlexibleContexts #-}
{-# OPTIONS -XUndecidableInstances #-}
{-# OPTIONS -XStandaloneDeriving #-}
{-# OPTIONS -XOverloadedStrings #-}
module Typeable.Tf2c4f6ddd939444ba209fbdf2152eb54 where
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
 
instance Data.EBF.EBF SchemeSymbol where
        get
          = do index <- Data.Binary.Get.getWord8
               case index of
                   0 -> (>>=) Data.EBF.get (\ a0 -> return (Latin a0))
                   1 -> (>>=) Data.EBF.get (\ a0 -> return (Decimal a0))
                   2 -> return Plus
                   3 -> return Minus
                   4 -> return FullStop
        put (Latin a)
          = do Data.Binary.Put.putWord8 0
               Data.EBF.put a
        put (Decimal a)
          = do Data.Binary.Put.putWord8 1
               Data.EBF.put a
        put Plus = do Data.Binary.Put.putWord8 2
        put Minus = do Data.Binary.Put.putWord8 3
        put FullStop = do Data.Binary.Put.putWord8 4
 
instance Data.Typeable.Typeable SchemeSymbol where
        typeOf _
          = Data.Typeable.mkTyConApp
              (Data.Typeable.mkTyCon
                 "Typeable.Tf2c4f6ddd939444ba209fbdf2152eb54.SchemeSymbol")
              []
 
instance Data.EBF.TypeIdent SchemeSymbol where
        typeOf _ = Data.Tree.Node "f2c4f6dd-d939-444b-a209-fbdf2152eb54" []