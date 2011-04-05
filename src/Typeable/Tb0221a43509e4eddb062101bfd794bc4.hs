{-# OPTIONS -XEmptyDataDecls #-}
{-# OPTIONS -XKindSignatures #-}
{-# OPTIONS -XNoImplicitPrelude #-}
{-# OPTIONS -XFlexibleContexts #-}
{-# OPTIONS -XUndecidableInstances #-}
{-# OPTIONS -XStandaloneDeriving #-}
module Typeable.Tb0221a43509e4eddb062101bfd794bc4 where
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
import qualified Typeable.T4f7db06c439541658a09689d3e7dd909
import qualified Typeable.Taf20e1db8f0d414f90625b1521e41378
import qualified Typeable.T9592f9fa4fae437a9e8d0917c14ff068
import qualified Typeable.T43c6cd1333b04fc8a480668ecb24768e
import qualified Typeable.T0ba85f3f10099c75d4b696d0cf944e09
import qualified Typeable.T34c13bdaac7d413ed735e64edcac7ff5
 
data StructuredText (a :: *) = Paragraph{paragraph ::
                                         Typeable.T43c6cd1333b04fc8a480668ecb24768e.Map
                                           Typeable.Taf20e1db8f0d414f90625b1521e41378.Language
                                           (Typeable.T0ba85f3f10099c75d4b696d0cf944e09.List
                                              (Typeable.T9592f9fa4fae437a9e8d0917c14ff068.TextElement
                                                 a))}
                             | IndentList{indentList ::
                                          Typeable.T0ba85f3f10099c75d4b696d0cf944e09.List
                                            (Typeable.Tb0221a43509e4eddb062101bfd794bc4.StructuredText
                                               a)}
                             | BulletList{bulletList ::
                                          Typeable.T0ba85f3f10099c75d4b696d0cf944e09.List
                                            (Typeable.Tb0221a43509e4eddb062101bfd794bc4.StructuredText
                                               a)}
                             | IndexedList{indexedList ::
                                           Typeable.T0ba85f3f10099c75d4b696d0cf944e09.List
                                             (Typeable.Tb0221a43509e4eddb062101bfd794bc4.StructuredText
                                                a)}
                             | TitledList{titledList ::
                                          Typeable.T0ba85f3f10099c75d4b696d0cf944e09.List
                                            (Typeable.T34c13bdaac7d413ed735e64edcac7ff5.Tuple
                                               (Typeable.T43c6cd1333b04fc8a480668ecb24768e.Map
                                                  Typeable.Taf20e1db8f0d414f90625b1521e41378.Language
                                                  Typeable.T4f7db06c439541658a09689d3e7dd909.Text)
                                               (Typeable.Tb0221a43509e4eddb062101bfd794bc4.StructuredText
                                                  a))}
 
deriving instance (Prelude.Eq a) => Prelude.Eq (StructuredText a)
 
deriving instance (Prelude.Ord a) => Prelude.Ord (StructuredText a)
 
deriving instance (Prelude.Show a) => Prelude.Show
         (StructuredText a)
 
instance (Data.EBF.EBF a) => Data.EBF.EBF (StructuredText a) where
        get
          = do index <- Data.Binary.Get.getWord8
               case index of
                   0 -> (>>=) Data.EBF.get (\ a0 -> return (Paragraph a0))
                   1 -> (>>=) Data.EBF.get (\ a0 -> return (IndentList a0))
                   2 -> (>>=) Data.EBF.get (\ a0 -> return (BulletList a0))
                   3 -> (>>=) Data.EBF.get (\ a0 -> return (IndexedList a0))
                   4 -> (>>=) Data.EBF.get (\ a0 -> return (TitledList a0))
        put (Paragraph a)
          = do Data.Binary.Put.putWord8 0
               Data.EBF.put a
        put (IndentList a)
          = do Data.Binary.Put.putWord8 1
               Data.EBF.put a
        put (BulletList a)
          = do Data.Binary.Put.putWord8 2
               Data.EBF.put a
        put (IndexedList a)
          = do Data.Binary.Put.putWord8 3
               Data.EBF.put a
        put (TitledList a)
          = do Data.Binary.Put.putWord8 4
               Data.EBF.put a
 
instance Data.Typeable.Typeable1 StructuredText where
        typeOf1 _
          = Data.Typeable.mkTyConApp
              (Data.Typeable.mkTyCon
                 "Typeable.Tb0221a43509e4eddb062101bfd794bc4.StructuredText")
              []
 
instance Data.EBF.TypeIdentS StructuredText where
        typeOfS _
          = Data.Tree.Node
              (UUID.UUID 234121198027222144668814280721031580612)
              []