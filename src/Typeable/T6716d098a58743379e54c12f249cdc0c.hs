{-# OPTIONS -XEmptyDataDecls #-}
{-# OPTIONS -XKindSignatures #-}
{-# OPTIONS -XNoImplicitPrelude #-}
{-# OPTIONS -XFlexibleContexts #-}
{-# OPTIONS -XUndecidableInstances #-}
{-# OPTIONS -XStandaloneDeriving #-}
{-# OPTIONS -XOverloadedStrings #-}
{-# OPTIONS -XDeriveDataTypeable #-}
module Typeable.T6716d098a58743379e54c12f249cdc0c where
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
 
data LatinAlphabet = A{}
                   | B{}
                   | C{}
                   | D{}
                   | E{}
                   | F{}
                   | G{}
                   | H{}
                   | I{}
                   | J{}
                   | K{}
                   | L{}
                   | M{}
                   | N{}
                   | O{}
                   | P{}
                   | Q{}
                   | R{}
                   | S{}
                   | T{}
                   | U{}
                   | V{}
                   | W{}
                   | X{}
                   | Y{}
                   | Z{}
 
deriving instance Prelude.Eq LatinAlphabet
 
deriving instance Prelude.Ord LatinAlphabet
 
deriving instance Prelude.Show LatinAlphabet
 
instance Data.EBF.EBF LatinAlphabet where
        get
          = do index <- Data.Binary.Get.getWord8
               case index of
                   0 -> return A
                   1 -> return B
                   2 -> return C
                   3 -> return D
                   4 -> return E
                   5 -> return F
                   6 -> return G
                   7 -> return H
                   8 -> return I
                   9 -> return J
                   10 -> return K
                   11 -> return L
                   12 -> return M
                   13 -> return N
                   14 -> return O
                   15 -> return P
                   16 -> return Q
                   17 -> return R
                   18 -> return S
                   19 -> return T
                   20 -> return U
                   21 -> return V
                   22 -> return W
                   23 -> return X
                   24 -> return Y
                   25 -> return Z
        put A = do Data.Binary.Put.putWord8 0
        put B = do Data.Binary.Put.putWord8 1
        put C = do Data.Binary.Put.putWord8 2
        put D = do Data.Binary.Put.putWord8 3
        put E = do Data.Binary.Put.putWord8 4
        put F = do Data.Binary.Put.putWord8 5
        put G = do Data.Binary.Put.putWord8 6
        put H = do Data.Binary.Put.putWord8 7
        put I = do Data.Binary.Put.putWord8 8
        put J = do Data.Binary.Put.putWord8 9
        put K = do Data.Binary.Put.putWord8 10
        put L = do Data.Binary.Put.putWord8 11
        put M = do Data.Binary.Put.putWord8 12
        put N = do Data.Binary.Put.putWord8 13
        put O = do Data.Binary.Put.putWord8 14
        put P = do Data.Binary.Put.putWord8 15
        put Q = do Data.Binary.Put.putWord8 16
        put R = do Data.Binary.Put.putWord8 17
        put S = do Data.Binary.Put.putWord8 18
        put T = do Data.Binary.Put.putWord8 19
        put U = do Data.Binary.Put.putWord8 20
        put V = do Data.Binary.Put.putWord8 21
        put W = do Data.Binary.Put.putWord8 22
        put X = do Data.Binary.Put.putWord8 23
        put Y = do Data.Binary.Put.putWord8 24
        put Z = do Data.Binary.Put.putWord8 25
 
instance Data.Typeable.Typeable LatinAlphabet where
        typeOf _
          = Data.Typeable.mkTyConApp
              (Data.Typeable.mkTyCon
                 "Typeable.T6716d098a58743379e54c12f249cdc0c.LatinAlphabet")
              []
 
instance Data.EBF.TypeIdent LatinAlphabet where
        typeOf _ = Data.Tree.Node "6716d098-a587-4337-9e54-c12f249cdc0c" []
 
deriving instance Prelude.Enum LatinAlphabet