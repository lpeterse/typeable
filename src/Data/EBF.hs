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

instance TypeIdent UUID where
  typeOf _ = Node (UUID 0x346674042a7248b4a94abff0726d0c43) []

instance TypeIdentS Tree where
  typeOfS _ = Node (UUID 0x964f0abad1bd422faaf57b6d9938db31) []

instance TypeIdent Bool where
  typeOf _ = Node (UUID 0x0219c59f732a8ef507215fbdb4cceacd) []

instance TypeIdentS Maybe where
  typeOfS _ = Node (UUID 0xf8f49ef6bbe874a42926fa23d5b3bc19) []

instance TypeIdentS [] where
  typeOfS _ = Node (UUID 0x0ba85f3f10099c75d4b696d0cf944e09) []

instance TypeIdentSS (,) where
  typeOfSS _ = Node (UUID 0x34c13bdaac7d413ed735e64edcac7ff5) []

instance TypeIdentS S.Set where
  typeOfS _ = Node (UUID 0x7af30cce93724981a16a80f3f193dc33) []

instance TypeIdentSS M.Map where
  typeOfSS _ = Node (UUID 0x43c6cd1333b04fc8a480668ecb24768e) []

instance TypeIdent ByteString where
  typeOf _ = Node (UUID 0xf9f2f27af0f649b4bc8946c467c3b76a) []

instance TypeIdent Integer where
  typeOf _ = Node (UUID 0x8006b4b18388f841272dbebeee847723) []

instance TypeIdent Text where
  typeOf _ = Node (UUID 0x4f7db06c439541658a09689d3e7dd909) []

instance TypeIdentS Ratio where  
  typeOfS _ = Node (UUID 0xc211e54d6eef4234a7b675d5f696efe5) []

instance TypeIdent Int where
  typeOf _ = Node (UUID 0xac2e770f2132aced749ec197385ff552) []

instance TypeIdent Word where
  typeOf _ = Node (UUID 0x62d2d5371f08461aa328bc06561594f6) []

instance TypeIdent Int8 where
  typeOf _ = Node (UUID 0xec78dc6268e4fe6fe6df461f40359d62) []

instance TypeIdent Int16 where
  typeOf _ = Node (UUID 0x7ee200d207963cca2d2a49719e97e973) []

instance TypeIdent Int32 where
  typeOf _ = Node (UUID 0x7b05ee3f0bbe6569f48d3947ec425493) []

instance TypeIdent Int64 where
  typeOf _ = Node (UUID 0xcc620c86261c781e03c8efd9a974b1cf) []

instance TypeIdent Word8 where
  typeOf _ = Node (UUID 0x7704e26b08886d6b8c3c788a3a0b2db0) []

instance TypeIdent Word16 where
  typeOf _ = Node (UUID 0xb567f4ccc26027e0a78edd227800fe94) []

instance TypeIdent Word32 where
  typeOf _ = Node (UUID 0x1a55145e5bd21e8adc14067707192552) []

instance TypeIdent Word64 where
  typeOf _ = Node (UUID 0x187e33b43715d8fe529de5014c864d85) []

instance TypeIdentSS LargeKey where
  typeOfSS = undefined

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

