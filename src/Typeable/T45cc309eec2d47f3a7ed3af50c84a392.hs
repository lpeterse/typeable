{-# OPTIONS -XEmptyDataDecls #-}
{-# OPTIONS -XKindSignatures #-}
{-# OPTIONS -XNoImplicitPrelude #-}
{-# OPTIONS -XFlexibleContexts #-}
{-# OPTIONS -XUndecidableInstances #-}
{-# OPTIONS -XStandaloneDeriving #-}
module Typeable.T45cc309eec2d47f3a7ed3af50c84a392 where
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
import qualified Typeable.T346674042a7248b4a94abff0726d0c43 as UUID
 
data HexadecimalAlphabet = Zero{}
                         | One{}
                         | Two{}
                         | Three{}
                         | Four{}
                         | Five{}
                         | Six{}
                         | Seven{}
                         | Eight{}
                         | Nine{}
                         | Ten{}
                         | Eleven{}
                         | Twelve{}
                         | Thirteen{}
                         | Fourteen{}
                         | Fifteen{}
 
deriving instance Prelude.Eq HexadecimalAlphabet
 
deriving instance Prelude.Ord HexadecimalAlphabet
 
deriving instance Prelude.Show HexadecimalAlphabet
 
instance Data.EBF.EBF HexadecimalAlphabet where
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
                   10 -> return Ten
                   11 -> return Eleven
                   12 -> return Twelve
                   13 -> return Thirteen
                   14 -> return Fourteen
                   15 -> return Fifteen
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
        put Ten = do Data.Binary.Put.putWord8 10
        put Eleven = do Data.Binary.Put.putWord8 11
        put Twelve = do Data.Binary.Put.putWord8 12
        put Thirteen = do Data.Binary.Put.putWord8 13
        put Fourteen = do Data.Binary.Put.putWord8 14
        put Fifteen = do Data.Binary.Put.putWord8 15
 
instance Data.Typeable.Typeable HexadecimalAlphabet where
        typeOf _
          = Data.Typeable.mkTyConApp
              (Data.Typeable.mkTyCon
                 "Typeable.T45cc309eec2d47f3a7ed3af50c84a392.HexadecimalAlphabet")
              []
 
instance Data.EBF.TypeIdent HexadecimalAlphabet where
        typeOf _
          = Data.Tree.Node (UUID.UUID 92776946415104156362928178444617687954)
              []
 
deriving instance Prelude.Enum HexadecimalAlphabet