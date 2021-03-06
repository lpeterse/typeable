{-# OPTIONS -XEmptyDataDecls #-}
{-# OPTIONS -XKindSignatures #-}
{-# OPTIONS -XNoImplicitPrelude #-}
{-# OPTIONS -XFlexibleContexts #-}
{-# OPTIONS -XUndecidableInstances #-}
{-# OPTIONS -XStandaloneDeriving #-}
{-# OPTIONS -XOverloadedStrings #-}
{-# OPTIONS -XDeriveDataTypeable #-}
module Typeable.T9790ade9814a4aaca5eaa80c3e47685d where
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
import qualified Typeable.T0ba85f3f10099c75d4b696d0cf944e09
import qualified Typeable.T1566edb1a4de4aab8106e63293e9bfcf
import qualified Typeable.T6716d098a58743379e54c12f249cdc0c
 
data Designator = Designator{initial ::
                             Typeable.T6716d098a58743379e54c12f249cdc0c.LatinAlphabet,
                             subsequent ::
                             Typeable.T0ba85f3f10099c75d4b696d0cf944e09.List
                               Typeable.T1566edb1a4de4aab8106e63293e9bfcf.Symbol}
 
deriving instance Prelude.Eq Designator
 
deriving instance Prelude.Ord Designator
 
deriving instance Prelude.Show Designator
 
instance Data.EBF.EBF Designator where
        get
          = do index <- return 0
               case index of
                   0 -> (>>=) Data.EBF.get
                          (\ a0 -> (>>=) Data.EBF.get (\ a1 -> return (Designator a0 a1)))
        put (Designator a b)
          = do Data.EBF.put a
               Data.EBF.put b
 
instance Data.Typeable.Typeable Designator where
        typeOf _
          = Data.Typeable.mkTyConApp
              (Data.Typeable.mkTyCon
                 "Typeable.T9790ade9814a4aaca5eaa80c3e47685d.Designator")
              []
 
instance Data.EBF.TypeIdent Designator where
        typeOf _ = Data.Tree.Node "9790ade9-814a-4aac-a5ea-a80c3e47685d" []