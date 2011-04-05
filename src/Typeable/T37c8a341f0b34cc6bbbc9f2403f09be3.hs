{-# OPTIONS -XEmptyDataDecls #-}
{-# OPTIONS -XKindSignatures #-}
{-# OPTIONS -XNoImplicitPrelude #-}
{-# OPTIONS -XFlexibleContexts #-}
{-# OPTIONS -XUndecidableInstances #-}
{-# OPTIONS -XStandaloneDeriving #-}
{-# OPTIONS -XOverloadedStrings #-}
module Typeable.T37c8a341f0b34cc6bbbc9f2403f09be3 where
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
import qualified Typeable.T0ba85f3f10099c75d4b696d0cf944e09
import qualified Typeable.T205895c8d2df475b8d5ead5ee33d9f63
import qualified Typeable.T2c62454c586f4bdea5e2b17e432db245
import qualified Typeable.T9790ade9814a4aaca5eaa80c3e47685d
import qualified Typeable.Tb0221a43509e4eddb062101bfd794bc4
 
data Constructor (a :: *) = Constructor{name ::
                                        Typeable.T9790ade9814a4aaca5eaa80c3e47685d.Designator,
                                        semantics ::
                                        Typeable.Tb0221a43509e4eddb062101bfd794bc4.StructuredText
                                          (Typeable.T2c62454c586f4bdea5e2b17e432db245.Extension a),
                                        fields ::
                                        Typeable.T0ba85f3f10099c75d4b696d0cf944e09.List
                                          (Typeable.T205895c8d2df475b8d5ead5ee33d9f63.Field a)}
 
deriving instance (Prelude.Eq a) => Prelude.Eq (Constructor a)
 
deriving instance (Prelude.Ord a) => Prelude.Ord (Constructor a)
 
deriving instance (Prelude.Show a) => Prelude.Show (Constructor a)
 
instance (Data.EBF.EBF a, Data.EBF.TypeIdent a) => Data.EBF.EBF
         (Constructor a) where
        get
          = do index <- return 0
               case index of
                   0 -> (>>=) Data.EBF.get
                          (\ a0 ->
                             (>>=) Data.EBF.get
                               (\ a1 ->
                                  (>>=) Data.EBF.get (\ a2 -> return (Constructor a0 a1 a2))))
        put (Constructor a b c)
          = do Data.EBF.put a
               Data.EBF.put b
               Data.EBF.put c
 
instance Data.Typeable.Typeable1 Constructor where
        typeOf1 _
          = Data.Typeable.mkTyConApp
              (Data.Typeable.mkTyCon
                 "Typeable.T37c8a341f0b34cc6bbbc9f2403f09be3.Constructor")
              []
 
instance Data.EBF.TypeIdentS Constructor where
        typeOfS _
          = Data.Tree.Node "37c8a341-f0b3-4cc6-bbbc-9f2403f09be3" []