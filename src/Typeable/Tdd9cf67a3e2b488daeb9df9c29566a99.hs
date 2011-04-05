{-# OPTIONS -XEmptyDataDecls #-}
{-# OPTIONS -XKindSignatures #-}
{-# OPTIONS -XNoImplicitPrelude #-}
{-# OPTIONS -XFlexibleContexts #-}
{-# OPTIONS -XUndecidableInstances #-}
{-# OPTIONS -XStandaloneDeriving #-}
module Typeable.Tdd9cf67a3e2b488daeb9df9c29566a99 where
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
import qualified Typeable.T26b9a53370bc4489a322192e2e0416ce
import qualified Typeable.T0ba85f3f10099c75d4b696d0cf944e09
 
data Turn (a :: *) = Turn{who ::
                          Typeable.T26b9a53370bc4489a322192e2e0416ce.SimpleSpeaker,
                          utterance :: Typeable.T0ba85f3f10099c75d4b696d0cf944e09.List a}
 
deriving instance (Prelude.Eq a) => Prelude.Eq (Turn a)
 
deriving instance (Prelude.Ord a) => Prelude.Ord (Turn a)
 
deriving instance (Prelude.Show a) => Prelude.Show (Turn a)
 
instance (Data.EBF.EBF a) => Data.EBF.EBF (Turn a) where
        get
          = do index <- return 0
               case index of
                   0 -> (>>=) Data.EBF.get
                          (\ a0 -> (>>=) Data.EBF.get (\ a1 -> return (Turn a0 a1)))
        put (Turn a b)
          = do Data.EBF.put a
               Data.EBF.put b
 
instance Data.Typeable.Typeable1 Turn where
        typeOf1 _
          = Data.Typeable.mkTyConApp
              (Data.Typeable.mkTyCon
                 "Typeable.Tdd9cf67a3e2b488daeb9df9c29566a99.Turn")
              []
 
instance Data.EBF.TypeIdentS Turn where
        typeOfS _
          = Data.Tree.Node
              (UUID.UUID 294574384536236563116100439603536095897)
              []