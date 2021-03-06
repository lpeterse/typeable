{-# OPTIONS -XEmptyDataDecls #-}
{-# OPTIONS -XKindSignatures #-}
{-# OPTIONS -XNoImplicitPrelude #-}
{-# OPTIONS -XFlexibleContexts #-}
{-# OPTIONS -XUndecidableInstances #-}
{-# OPTIONS -XStandaloneDeriving #-}
{-# OPTIONS -XOverloadedStrings #-}
{-# OPTIONS -XDeriveDataTypeable #-}
module Typeable.Tff421b2c31774c37a7336c8245a74da9 where
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
 
data DecimalAlphabet = Zero{}
                     | One{}
                     | Two{}
                     | Three{}
                     | Four{}
                     | Five{}
                     | Six{}
                     | Seven{}
                     | Eight{}
                     | Nine{}
 
deriving instance Prelude.Eq DecimalAlphabet
 
deriving instance Prelude.Ord DecimalAlphabet
 
deriving instance Prelude.Show DecimalAlphabet
 
instance Data.EBF.EBF DecimalAlphabet where
        get
          = do index <- Data.Binary.Get.getWord8
               case index of
                   0 -> return Zero
                   1 -> return One
                   2 -> return Two
                   3 -> return Three
                   4 -> return Four
                   5 -> return Five
                   6 -> return Six
                   7 -> return Seven
                   8 -> return Eight
                   9 -> return Nine
        put Zero = do Data.Binary.Put.putWord8 0
        put One = do Data.Binary.Put.putWord8 1
        put Two = do Data.Binary.Put.putWord8 2
        put Three = do Data.Binary.Put.putWord8 3
        put Four = do Data.Binary.Put.putWord8 4
        put Five = do Data.Binary.Put.putWord8 5
        put Six = do Data.Binary.Put.putWord8 6
        put Seven = do Data.Binary.Put.putWord8 7
        put Eight = do Data.Binary.Put.putWord8 8
        put Nine = do Data.Binary.Put.putWord8 9
 
instance Data.Typeable.Typeable DecimalAlphabet where
        typeOf _
          = Data.Typeable.mkTyConApp
              (Data.Typeable.mkTyCon
                 "Typeable.Tff421b2c31774c37a7336c8245a74da9.DecimalAlphabet")
              []
 
instance Data.EBF.TypeIdent DecimalAlphabet where
        typeOf _ = Data.Tree.Node "ff421b2c-3177-4c37-a733-6c8245a74da9" []
 
deriving instance Prelude.Enum DecimalAlphabet