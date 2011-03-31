{-# OPTIONS -XFlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Typeable.Internal.InternalTypeDefs where

import Typeable.T6716d098a58743379e54c12f249cdc0c --LatinAlphabet
import Typeable.Tff421b2c31774c37a7336c8245a74da9 --DecimalAlphabet
import Typeable.T9790ade9814a4aaca5eaa80c3e47685d --Designator
import Typeable.T1566edb1a4de4aab8106e63293e9bfcf --Symbol
import Typeable.Tb0221a43509e4eddb062101bfd794bc4 --StructuredText
import Typeable.T9592f9fa4fae437a9e8d0917c14ff068 --TextElement
import Typeable.T2c62454c586f4bdea5e2b17e432db245 (Extension) --Extension
import Typeable.Taf20e1db8f0d414f90625b1521e41378 --Language
import Typeable.T9e2e1e478e094a8abe5507f8574ac91f --Succ
import Typeable.T421496848904471ea3197f25e2a02b72 --Zero
import Typeable.T606f253533d3420da3465afae341d598 --Time
import Typeable.Tc1b1f6c722c2436fab3180146520814e --UTC
import Typeable.T1660b01f08dc4aedbe4c0941584541cb --Kind
import Typeable.T346674042a7248b4a94abff0726d0c43 --UUID

import Numeric
import Data.Word
import Data.LargeWord
import Data.Char
import Data.String
import qualified Data.Text as T
import qualified Data.Set  as S
import qualified Data.Map  as M
import Data.Ratio
import Data.Time
import Data.Time.Calendar
import Data.Time.Clock
import System.Locale

import Data.EBF

class (Ord a, Enum a) => PeanoNumber a where
  domain :: [a]

instance PeanoNumber Zero where
  domain = []

instance PeanoNumber a => PeanoNumber (Succ a) where
  domain = case domain :: [a] of
            [] -> [First]
            xs -> First:(map Next xs)

instance PeanoNumber k => Enum (Succ k) where
  fromEnum First      = 0
  fromEnum (Next x)   = 1 + (fromEnum x)
  toEnum 0            = First
  toEnum n            = Next (toEnum (n-1))

-- Show' instances
---------------------------------------------------------------------------------------------------

class (Show a) => Show' a where
  show' :: a -> String
  show'  = show

instance Show' UUID where
  show' (UUID x) = f $ showHex (fromIntegral x :: Integer) ""
                   where
                     f xs | length xs < 32 = f ('0':xs)
                          | otherwise        = xs

instance Show' Kind where
  show' x = show'' x False
    where
      show'' KindStar _                   = "*"
      show'' z@(KindApplication _ _) True = "("++(show'' z False)++")" 
      show'' (KindApplication a b) False  = (show'' a True) ++ " -> " ++ (show'' b False)

instance Show' [Symbol] where
  show' []              = []
  show' (Underscore:xs) = '_':(show' xs)
  show' (Prime     :xs) = '\'':(show' xs)
  show' (Lower a   :xs) = (chr $ (fromEnum a)+97):(show' xs)
  show' (Upper a   :xs) = (chr $ (fromEnum a)+65):(show' xs)
  show' (Decimal a :xs) = (chr $ (fromEnum a)+48):(show' xs)

instance Show' Designator where
  show' (Designator a xs) = (chr $ (fromEnum a)+65):(show' xs)

instance Show' (Time UTC) where
  show' (Time x) = formatTime defaultTimeLocale "%c" $ UTCTime (ModifiedJulianDay (fromIntegral d)) 
                                                               (secondsToDiffTime (fromIntegral s)) 
                  where
                    y = (numerator x `div` denominator x)+1297728000
                    (d,s) = y `quotRem` (3600*24)

-- IsString instances
---------------------------------------------------------------------------------------------------

instance IsString UUID where
  fromString = UUID . fromInteger . fst . head . readHex . (filter isHexDigit)

instance PeanoNumber k => IsString (StructuredText (Extension k)) where
  fromString [] = Paragraph M.empty
  fromString xs = Paragraph $ M.singleton ENG [Plaintext (T.pack xs) False False False False]

instance IsString Designator where
  fromString []     = error "Designator must consist of at least one letter."
  fromString (x:xs) | i < 97 || i > 122 = error $ "Character '"++(show x)++"' is not a letter."
                    | otherwise         = Designator (toEnum $ i-97) (fromString xs)  
                    where 
                      i = fromEnum (toLower x)

instance IsString [Symbol] where
  fromString []        = []
  fromString ('_':xs)  = Underscore:(fromString xs) 
  fromString ('\'':xs) = Prime:(fromString xs)
  fromString (x  :xs)  | i >= 48 && i <= 57  = (Decimal $ toEnum $ i-48):(fromString xs)
                       | i >= 65 && i <= 90  = (Upper   $ toEnum $ i-65):(fromString xs)
                       | i >= 97 && i <= 122 = (Lower   $ toEnum $ i-97):(fromString xs)
                       | otherwise           = error $ "Character '"++(show x)++"' is not"
                                                       ++" allowed in a Designator."
                       where i = fromEnum x

data Namespace = Namespace {
                             nstypes      :: S.Set UUID
                            ,nsclasses    :: S.Set UUID
                            ,subspaces    :: M.Map Designator Namespace
                           } deriving (Eq, Show)
                           
