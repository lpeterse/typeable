{-# OPTIONS -XOverloadedStrings -XFlexibleInstances -XKindSignatures -XStandaloneDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Data.EBF 
       (
         module Data.EBF.TypeIdent
       , EBF (..)
       , headerV00
       , readTypeV00
       , readV00
       , writeV00
       )
       where

import Data.EBF.TypeIdent 

import qualified Data.Binary     as B
import qualified Data.Binary.Put as BP
import qualified Data.Binary.Get as BG
import qualified Data.Map        as M
import qualified Data.Set        as S
import Data.Text hiding (take, drop)
import Data.UUID
import Data.Text.Encoding
import Data.ByteString hiding (take, drop)
import Data.Word 
import Data.Int
import Data.Tree
import Data.Ratio
import Data.Char
import Data.String
import Data.LargeWord
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Internal as LI
import qualified Crypto.Hash.Skein256 as Hash

class (Ord a, TypeIdent a) => EBF a where
  put :: a -> B.Put
  get :: B.Get a

headerV00           :: LBS.ByteString
headerV00            = LBS.pack [0x1b, 0x5b, 0x31, 0x3b, 
                                 0x33, 0x35, 0x6d, 0x45, 
                                 0x42, 0x46, 0x30, 0x30, 
                                 0x0d, 0x0a, 0x1a, 0x0a]

readTypeV00         :: LBS.ByteString -> Tree UUID
readTypeV00        x = if not (headerV00 `LBS.isPrefixOf` x)
                          then error "Can't find valid EBF header."
                          else BG.runGet get (LBS.drop 48 x)

readV00  :: (EBF a) => LBS.ByteString -> a
readV00 x = if not (headerV00 `LBS.isPrefixOf` x)
              then error "Can't find valid EBF header."
              else BG.runGet g (LBS.drop 48 x)
              where
                g = do get :: B.Get (Tree UUID)
                       get

writeV00        :: (EBF a) => a -> LBS.ByteString 
writeV00 x       = headerV00 `LBS.append` hash `LBS.append` content
                   where
                     content = BP.runPut $ do put (typeOf x)
                                              put x
                     hash    = LI.Chunk (Hash.hashlazy 256 content) LI.Empty


-- EBF instances for common and abstract types
------------------------------------------------

deriving instance (Ord a) => Ord (Tree a)

instance EBF UUID where
  get = B.get
  put = B.put

instance (EBF a) => EBF (Tree a) where
  put (Node a b) = put a >> put b
  get            = get >>= \a-> get >>= \b-> return (Node a b) 
 
instance EBF Bool where
  put False = BP.putWord8 0
  put True  = BP.putWord8 1
  get       = BG.getWord8 >>= \x-> case x of
                                     0 -> return False
                                     1 -> return True

instance EBF Char where
  put = B.put
  get = B.get

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

instance (EBF a, EBF b) => EBF (Either a b) where
        get
          = do index <- BG.getWord8
               case index of
                   0 -> (>>=) get (\ a0 -> return (Left a0))
                   1 -> (>>=) get (\ a0 -> return (Right a0))
        put (Left a)
          = do BP.putWord8 0
               put a
        put (Right a)
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

