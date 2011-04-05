{-# OPTIONS -XFlexibleInstances -XKindSignatures -XStandaloneDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Data.EBF where

import Typeable.T346674042a7248b4a94abff0726d0c43

import qualified Data.Binary as B
import qualified Data.Binary.Put as BP
import qualified Data.Binary.Get as BG
import qualified Data.Map    as M
import qualified Data.Set    as S
import Data.Text
import Data.Text.Encoding
import Data.ByteString
import Data.Word 
import Data.Int
import Data.Tree
import Data.Ratio
import Data.LargeWord

class (Ord a) => EBF a where
  put :: a -> B.Put
  get :: B.Get a

deriving instance (Ord a) => Ord (Tree a)

---------------------------------------

instance EBF UUID where
        get
          = do index <- return 0
               case index of
                   0 -> (>>=) get (\ a0 -> return (UUID a0))
        put (UUID a) = do put a

instance (EBF a) => EBF (Tree a) where
  put (Node a b) = put a >> put b
  get            = get >>= \a-> get >>= \b-> return (Node a b) 
 
instance EBF Bool where
  put False = BP.putWord8 0
  put True  = BP.putWord8 1
  get       = BG.getWord8 >>= \x-> case x of
                                     0 -> return False
                                     1 -> return True

instance (EBF a) => EBF (Maybe a) where
        get
          = do index <- BG.getWord8
               case index of
                   0 -> return Nothing
                   1 -> (>>=) get (\ a0 -> return (Just a0))
        put Nothing = do BP.putWord8 0
        put (Just a)
          = do BP.putWord8 1
               put a

instance (EBF a) => EBF [a] where
  put []     = BP.putWord8 1
  put (x:xs) = BP.putWord8 0 >> put x >> put xs 
  get        = BG.getWord8 >>= \x-> case x of
                                      1 -> return []
                                      0 -> get >>= \a-> get >>= \b-> return (a:b)

instance (EBF a, EBF b) => EBF ((a,b)) where
  put (a,b) = put a >> put b
  get       = get >>= \a-> get >>= \b-> return (a,b)

instance (EBF a) => EBF (S.Set a) where
  put = put . S.toAscList
  get = get >>= (return . S.fromDistinctAscList)

instance (EBF k, EBF a) => EBF (M.Map k a) where
  put = put . M.toAscList 
  get = get >>= (return . M.fromDistinctAscList)

instance EBF ByteString where
  put = B.put
  get = B.get

instance EBF Integer where
  put = B.put
  get = B.get

instance EBF Text where
  put = put . encodeUtf8
  get = get >>= (return . decodeUtf8)

instance (Integral a, EBF a) => EBF (Ratio a) where
  put x = put (numerator x) >> put (denominator x)
  get   = get >>= \a-> get >>= \b-> return (a % b)

instance EBF Int where
  put = B.put
  get = B.get

instance EBF Word where
  put = B.put
  get = B.get

instance EBF Int8 where
  put = B.put
  get = B.get

instance EBF Int16 where
  put = B.put
  get = B.get

instance EBF Int32 where
  put = B.put
  get = B.get

instance EBF Int64 where
  put = B.put
  get = B.get

instance EBF Word8 where
  put = B.put
  get = B.get

instance EBF Word16 where
  put = B.put
  get = B.get

instance EBF Word32 where
  put = B.put
  get = B.get

instance EBF Word64 where
  put = B.put
  get = B.get

instance EBF (LargeKey Word64 Word64) where
  put x = BP.putWord64be (hiHalf x) >> BP.putWord64be (loHalf x)
  get   = BG.getWord64be >>= \h-> BG.getWord64be >>= \l-> return (fromIntegral (((fromIntegral h)*2^64+(fromIntegral l))::Integer))

-------------------------------------------

applyTo    :: Tree a -> Tree a -> Tree a
applyTo x y = x { subForest = (subForest x) ++ [y] } 

class TypeIdent a where
  typeOf :: a -> Tree UUID

class TypeIdentS t where
  typeOfS :: t a -> Tree UUID

class TypeIdentASS t where
  typeOfASS :: t (a :: * -> * ) -> Tree UUID

class TypeIdentSS t where
  typeOfSS :: t a b -> Tree UUID

class TypeIdentSSS t where
  typeOfSSS :: t a b c -> Tree UUID

class TypeIdentSSSS t where
  typeOfSSSS :: t a b c d -> Tree UUID

class TypeIdentSSSSS t where
  typeOfSSSSS :: t a b c d e -> Tree UUID

class TypeIdentSSSSSS t where
  typeOfSSSSSS :: t a b c d e f -> Tree UUID

class TypeIdentSSSSSSS t where
  typeOfSSSSSSS :: t a b c d e f g -> Tree UUID

-----------------------------------------------

typeOfSDefault :: forall t a. (TypeIdentS t, TypeIdent a) => t a -> Tree UUID
typeOfSDefault = \_ -> rep
 where
   rep = typeOfS (undefined :: t a) `applyTo` 
         typeOf  (undefined :: a)

typeOfASSDefault :: forall t a. (TypeIdentASS t, TypeIdentS a) => t a -> Tree UUID
typeOfASSDefault = \_ -> rep
 where
   rep = typeOfASS (undefined :: t a) `applyTo` 
         typeOfS  (undefined :: a ())

typeOfSSDefault :: forall t a b. (TypeIdentSS t, TypeIdent a) => t a b -> Tree UUID
typeOfSSDefault = \_ -> rep 
 where
   rep = typeOfSS (undefined :: t a b) `applyTo` 
         typeOf  (undefined :: a)

typeOfSSSDefault :: forall t a b c. (TypeIdentSSS t, TypeIdent a) => t a b c -> Tree UUID
typeOfSSSDefault = \_ -> rep 
 where
   rep = typeOfSSS (undefined :: t a b c) `applyTo` 
         typeOf  (undefined :: a)

typeOfSSSSDefault :: forall t a b c d. (TypeIdentSSSS t, TypeIdent a) => t a b c d -> Tree UUID
typeOfSSSSDefault = \_ -> rep
 where
   rep = typeOfSSSS (undefined :: t a b c d) `applyTo` 
         typeOf  (undefined :: a)
   
typeOfSSSSSDefault :: forall t a b c d e. (TypeIdentSSSSS t, TypeIdent a) => t a b c d e -> Tree UUID
typeOfSSSSSDefault = \_ -> rep 
 where
   rep = typeOfSSSSS (undefined :: t a b c d e) `applyTo` 
         typeOf  (undefined :: a)

typeOfSSSSSSDefault :: forall t a b c d e f. (TypeIdentSSSSSS t, TypeIdent a) => t a b c d e f -> Tree UUID
typeOfSSSSSSDefault = \_ -> rep
 where
   rep = typeOfSSSSSS (undefined :: t a b c d e f) `applyTo` 
         typeOf  (undefined :: a)

typeOfSSSSSSSDefault :: forall t a b c d e f g. (TypeIdentSSSSSSS t, TypeIdent a) => t a b c d e f g -> Tree UUID
typeOfSSSSSSSDefault = \_ -> rep
 where
   rep = typeOfSSSSSSS (undefined :: t a b c d e f g) `applyTo` 
         typeOf  (undefined :: a)

----------------------------------

instance (TypeIdentS s, TypeIdent a) => TypeIdent (s a) where
  typeOf = typeOfSDefault

instance (TypeIdentASS s, TypeIdentS a) => TypeIdent (s a) where
  typeOf = typeOfASSDefault

instance (TypeIdentSS s, TypeIdent a) => TypeIdentS (s a) where
  typeOfS = typeOfSSDefault

instance (TypeIdentSSS s, TypeIdent a) => TypeIdentSS (s a) where
  typeOfSS = typeOfSSSDefault

instance (TypeIdentSSSS s, TypeIdent a) => TypeIdentSSS (s a) where
  typeOfSSS = typeOfSSSSDefault

instance (TypeIdentSSSSS s, TypeIdent a) => TypeIdentSSSS (s a) where
  typeOfSSSS = typeOfSSSSSDefault

instance (TypeIdentSSSSSS s, TypeIdent a) => TypeIdentSSSSS (s a) where
  typeOfSSSSS = typeOfSSSSSSDefault

instance (TypeIdentSSSSSSS s, TypeIdent a) => TypeIdentSSSSSS (s a) where
  typeOfSSSSSS = typeOfSSSSSSSDefault

