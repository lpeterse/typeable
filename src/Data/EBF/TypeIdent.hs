{-# OPTIONS -XOverloadedStrings -XKindSignatures -XFlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Data.EBF.TypeIdent where

import Data.Tree
import Data.UUID 
import Data.String
import Data.ByteString hiding (take, drop)
import Data.Word
import Data.Int
import Data.Ratio
import Data.Char
import Data.Text hiding (take, drop) 
import Data.LargeWord
import qualified Data.Map        as M
import qualified Data.Set        as S

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

-- instances for common and abstract types
------------------------------------------------------

instance TypeIdent UUID where
  typeOf _ = Node "346674042a7248b4a94abff0726d0c43" []

instance TypeIdentS Tree where
  typeOfS _ = Node "964f0abad1bd422faaf57b6d9938db31" []

instance TypeIdent Bool where
  typeOf _ = Node "0219c59f732a8ef507215fbdb4cceacd" []

instance TypeIdentS Maybe where
  typeOfS _ = Node "f8f49ef6bbe874a42926fa23d5b3bc19" []

instance TypeIdentSS Either where
  typeOfSS _ = Node "d9eef038-b47d-0820-c160-ceb8b6a89943" []

instance TypeIdentS [] where
  typeOfS _ = Node "0ba85f3f10099c75d4b696d0cf944e09" []

instance TypeIdent () where
  typeOf _ = Node "10f280df659654becb6e08122e846284" []

instance TypeIdentSS (,) where
  typeOfSS _ = Node "34c13bdaac7d413ed735e64edcac7ff5" []

instance TypeIdentS S.Set where
  typeOfS _ = Node "7af30cce93724981a16a80f3f193dc33" []

instance TypeIdentSS M.Map where
  typeOfSS _ = Node "43c6cd1333b04fc8a480668ecb24768e" []

instance TypeIdent ByteString where
  typeOf _ = Node "f9f2f27af0f649b4bc8946c467c3b76a" []

instance TypeIdent Integer where
  typeOf _ = Node "8006b4b18388f841272dbebeee847723" []

instance TypeIdent Text where
  typeOf _ = Node "4f7db06c439541658a09689d3e7dd909" []

instance TypeIdentS Ratio where  
  typeOfS _ = Node "c211e54d6eef4234a7b675d5f696efe5" []

instance TypeIdent Char where  
  typeOf _ = Node "16f4245df3cc0b534f028235ff8aae16" []

instance TypeIdent Int where
  typeOf _ = Node "ac2e770f2132aced749ec197385ff552" []

instance TypeIdent Word where
  typeOf _ = Node "62d2d5371f08461aa328bc06561594f6" []

instance TypeIdent Int8 where
  typeOf _ = Node "ec78dc6268e4fe6fe6df461f40359d62" []

instance TypeIdent Int16 where
  typeOf _ = Node "7ee200d207963cca2d2a49719e97e973" []

instance TypeIdent Int32 where
  typeOf _ = Node "7b05ee3f0bbe6569f48d3947ec425493" []

instance TypeIdent Int64 where
  typeOf _ = Node "cc620c86261c781e03c8efd9a974b1cf" []

instance TypeIdent Word8 where
  typeOf _ = Node "7704e26b08886d6b8c3c788a3a0b2db0" []

instance TypeIdent Word16 where
  typeOf _ = Node "b567f4ccc26027e0a78edd227800fe94" []

instance TypeIdent Word32 where
  typeOf _ = Node "1a55145e5bd21e8adc14067707192552" []

instance TypeIdent Word64 where
  typeOf _ = Node "187e33b43715d8fe529de5014c864d85" []

instance TypeIdentSS LargeKey where
  typeOfSS = undefined

instance TypeIdent Float where
  typeOf _ = Node "c74c35ddb3ef689646c50be868d11bdf" []

instance TypeIdent Double where
  typeOf _ = Node "4b19d19d959322ac0ccd319a4d275bd0" []

-- unspecific instances
--------------------------------------------------------

applyTo    :: Tree a -> Tree a -> Tree a
applyTo x y = x { subForest = (subForest x) ++ [y] } 


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


-- isString instance for UUID
---------------------------------

instance IsString UUID where
  fromString xs = case Data.UUID.fromString $ a++"-"++b++"-"++c++"-"++d++"-"++e of
                    Just z -> z
                    Nothing -> error "can't read UUID"
                  where
                    ys = Prelude.filter isHexDigit xs
                    a  = take 8  $ ys
                    b  = take 4  $ drop 8 ys
                    c  = take 4  $ drop 12 ys
                    d  = take 4  $ drop 16 ys
                    e  = take 12 $ drop 20 ys


