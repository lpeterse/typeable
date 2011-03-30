{-# OPTIONS -XEmptyDataDecls -XDeriveDataTypeable -XStandaloneDeriving -XFlexibleInstances -XOverloadedStrings #-}
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
import Typeable.T0174bd2264004820bfe34e211cb35a7d --DataType
import Typeable.T2a94a7a8d4e049759d8dd546e72293ff --Constraint

import Data.Word
import Data.LargeWord
import GHC.Real
import Numeric
import Data.String
import Data.Text (Text)
import qualified Data.Text as T
import Data.Map
import Data.Set
import qualified Data.Set as S
import Data.ByteString
import Data.Ratio
import Data.Time
import Data.Time.Calendar
import Data.Time.Clock
import System.Locale
import Network.URL
import Data.Char
import Prelude as P

import qualified Data.Map as M

import Typeable.Internal.EBF

type List a = [a]

instance Show' (Time UTC) where
  show' (Time x) = formatTime defaultTimeLocale "%c" $ UTCTime (ModifiedJulianDay (fromIntegral d)) (secondsToDiffTime (fromIntegral s)) 
                  where
                    y = (numerator x `div` denominator x)+1297728000
                    (d,s) = y `quotRem` (3600*24)

class (Ord a, Enum a) => PeanoNumber a where
  domain :: [a]

instance PeanoNumber Zero where
  domain = []

instance PeanoNumber a => PeanoNumber (Succ a) where
  domain = case domain :: [a] of
            [] -> [First]
            xs -> First:(P.map Next xs)

instance PeanoNumber k => Enum (Succ k) where
  fromEnum First      = 0
  fromEnum (Next x)   = 1 + (fromEnum x)
  toEnum 0            = First
  toEnum n            = Next (toEnum (n-1))

isAbstract :: Definition Type' -> Bool
isAbstract  = f . structure
              where
                f               :: (PeanoNumber a) => Binding a b Type' -> Bool
                f (Bind _ b)     = f b
                f (Expression t) = (constructors t) == Nothing

instance Show' Kind where
  show' x = show'' x False
    where
      show'' KindStar _               = "*"
      show'' z@(KindApplication _ _) True = "("++(show'' z False)++")" 
      show'' (KindApplication a b) False  = (show'' a True) ++ " -> " ++ (show'' b False)

var2String :: (PeanoNumber k) => k -> String
var2String x = [toEnum (97 + fromEnum x)]

instance IsString UUID where
  fromString = UUID . fromInteger . fst . P.head . readHex . (P.filter isHexDigit)

instance Show' UUID where
  show' (UUID x) = f $ showHex (fromIntegral x :: Integer) ""
                   where
                     f xs | P.length xs < 32 = f ('0':xs)
                          | otherwise        = xs

data Person = Person    { personName :: String
                        } 
                        deriving (Eq, Ord, Show)

data Definition a = Definition
                    { identifier       :: UUID
                    , antecedent       :: Maybe UUID
                    , name             :: Designator
                    , creationTime     :: Time UTC 
                    , modificationTime :: Time UTC
                    , author           :: Maybe Person
                    , maintainer       :: Person
                    , structure        :: Binding Zero Kind a
                    }

data Type' a      = Type'
                    { semantics        :: Text
                    , constraints      :: Set (Constraint a)
                    , constructors     :: Maybe [Constructor a]
                    }

data Class' k = Class'
                        { classSemantics       :: Annotation k
                        , classConstraints     :: Set (Constraint k)
                        , classMethods         :: [Method k]
                        }
                        deriving (Eq, Ord, Show)

data Method k = Method
                        { methodName      :: Designator
                        , methodSignature :: DataType k
                        , methodSemantics :: Annotation k
                        }
                        deriving (Eq, Ord, Show)

data Constructor k = Constructor
                        { constructorName      :: Designator
                        , constructorSemantics :: Annotation k
                        , constructorFields    :: [Field k] 
                        }
                        deriving (Eq, Ord, Show)

data Field k = Field
                        { fieldName      :: Designator
                        , fieldSemantics :: Annotation k
                        , fieldType      :: DataType k
                        }
                        deriving (Eq, Ord, Show)

data Binding a b c = Bind { associated :: b, bound :: (Binding (Succ a) b c) }
                                      | Expression { expression :: c a }

type Annotation a = StructuredText (Extension a)

instance PeanoNumber k => IsString (StructuredText (Extension k)) where
  fromString [] = Paragraph M.empty
  fromString xs = Paragraph $ M.singleton ENG [Plaintext (T.pack xs) False False False False]

---------------------------------------------------------------------------------------------

class (Show a) => Show' a where
  show' :: a -> String
  show'  = show

instance Show' [Symbol] where
  show' []              = []
  show' (Underscore:xs) = '_':(show' xs)
  show' (Prime     :xs) = '\'':(show' xs)
  show' (Lower a   :xs) = (chr $ (fromEnum a)+97):(show' xs)
  show' (Upper a   :xs) = (chr $ (fromEnum a)+65):(show' xs)
  show' (Decimal a :xs) = (chr $ (fromEnum a)+48):(show' xs)

instance Show' Designator where
  show' (Designator a xs) = (chr $ (fromEnum a)+65):(show' xs)

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
                       | otherwise           = error $ "Character '"++(show x)++"' is not allowed here.'"
                       where i = fromEnum x

data Namespace = Namespace {
                             nstypes      :: S.Set UUID
                            ,nsclasses    :: S.Set UUID
                            ,subspaces    :: M.Map Designator Namespace
                           } deriving (Eq, Show)
                           
