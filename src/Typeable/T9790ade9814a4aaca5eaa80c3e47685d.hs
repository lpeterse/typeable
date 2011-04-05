{-# OPTIONS -XEmptyDataDecls #-}
{-# OPTIONS -XKindSignatures #-}
{-# OPTIONS -XNoImplicitPrelude #-}
{-# OPTIONS -XFlexibleContexts #-}
{-# OPTIONS -XUndecidableInstances #-}
{-# OPTIONS -XStandaloneDeriving #-}
module Typeable.T9790ade9814a4aaca5eaa80c3e47685d where
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
import qualified Typeable.T1566edb1a4de4aab8106e63293e9bfcf
import qualified Typeable.T6716d098a58743379e54c12f249cdc0c
import qualified Typeable.T0ba85f3f10099c75d4b696d0cf944e09
 
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
        typeOf _
          = Data.Tree.Node
              (UUID.UUID 201464645468187988967077522131512682589)
              []