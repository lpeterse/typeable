{-# OPTIONS -XEmptyDataDecls -XDeriveDataTypeable -XStandaloneDeriving -XFlexibleInstances -XOverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Typeable.Internal.InternalTypeDefs where


import Data.Word
import Data.LargeWord
import GHC.Real
import Numeric
import Data.String
import Data.Text
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

import Data.Typeable
import Data.Data

import qualified Data.Map as M

import Typeable.Internal.EBF

type List a = [a]

data Time a = Time { seconds :: Rational } deriving (Eq, Ord)

data UTC

instance Show (Time UTC) where
  show (Time x) = formatTime defaultTimeLocale "%c" $ UTCTime (ModifiedJulianDay (fromIntegral d)) (secondsToDiffTime (fromIntegral s)) 
                  where
                    y = (numerator x `div` denominator x)+1297728000
                    (d,s) = y `quotRem` (3600*24)

instance EBF (Time a) where
  put = error "Time: no instance for EBF"
  get = error "Time: no instance for EBF"

class (Finite a) => PeanoNumber a

instance PeanoNumber Zero
instance PeanoNumber a => PeanoNumber (Succ a)

class (Eq a, Ord a, Enum a) => Finite a where
  domain :: [a]

data Zero

instance Eq Zero where
  (==) = undefined
 
instance Ord Zero where
  compare = undefined

instance Show Zero where
  show = undefined

instance Enum Zero where
  fromEnum = undefined
  toEnum   = undefined

instance Finite Zero where
  domain = []

data Succ a = First | Next a deriving (Eq, Ord, Show, Read)

instance PeanoNumber k => Enum (Succ k) where
  fromEnum First      = 0
  fromEnum (Next x)   = 1 + (fromEnum x)
  toEnum 0            = First
  toEnum n            = Next (toEnum (n-1))

instance (PeanoNumber a, Finite a) => Finite (Succ a) where
  domain = case domain :: [a] of
            [] -> [First]
            xs -> First:(P.map Next xs)

---------------------


isAbstract :: Definition Type' -> Bool
isAbstract  = f . structure
              where
                f               :: (PeanoNumber a) => Binding a b Type' -> Bool
                f (Bind _ b)     = f b
                f (Expression t) = (constructors t) == Nothing

data Kind' = Concrete'
           | Application' Kind' Kind'
           deriving (Eq, Ord)

instance Show Kind' where
  show x = show' x False
    where
      show' Concrete' _ = "*"
      show' z@(Application' _ _) True = "("++(show' z False)++")" 
      show' (Application' a b) False = (show' a True) ++ " -> " ++ (show' b False)

var2String :: (PeanoNumber k) => k -> String
var2String x = [toEnum (97 + fromEnum x)]

instance IsString UUID where
  fromString = UUID . fromInteger . fst . P.head . readHex . (P.filter isHexDigit)

instance PeanoNumber k => IsString (Annotation k) where
  fromString [] = Plain ""
  fromString xs = Plain (fromString xs)

data UUID       = UUID Word128 deriving (Eq, Ord)

instance Show UUID where
  show (UUID x) = f $ showHex (fromIntegral x :: Integer) ""
                  where
                    f xs | P.length xs < 32 = f ('0':xs)
                         | otherwise        = xs


data (PeanoNumber k)        => Type k = DataType      { typeRef ::  UUID } -- entweder extern der Ordnung k
                               | Variable      k                    -- oder freie Variable der Ordnung k
                               | Application   (Type k) (Type k)    -- oder Typkonstruktor höherer Ordnung wird angewandt
                               | Forall        { quantified :: (Type (Succ k)) }
                               deriving (Eq, Ord, Show)

data (PeanoNumber k) => Constraint k = Constraint
                                       { constraintClass :: UUID
                                       , constraintVars  :: [Type k]
                                       }
                                       deriving (Eq, Ord, Show)

data Person = Person    { personName :: String
                        } 
                        deriving (Eq, Ord, Show)

data Definition a = Definition
                    { identifier       :: UUID
                    , antecedent       :: Maybe UUID
                    , name             :: UpperDesignator
                    , creationTime     :: Time UTC 
                    , modificationTime :: Time UTC
                    , author           :: Maybe Person
                    , maintainer       :: Person
                    , structure        :: Binding Zero Kind' a
                    }

data (PeanoNumber a) => Type' a      = Type'
                    { semantics        :: Text
                    , constraints      :: Set (Constraint a)
                    , constructors     :: Maybe [Constructor a]
                    }

data (PeanoNumber k) => Class' k = Class'
                        { classSemantics       :: Annotation k
                        , classConstraints     :: Set (Constraint k)
                        , classMethods         :: [Method k]
                        }
                        deriving (Eq, Ord, Show)

data (PeanoNumber k) => Method k = Method
                        { methodName      :: LowerDesignator
                        , methodSignature :: Type k
                        , mehtodSemantics :: Annotation k
                        }
                        deriving (Eq, Ord, Show)

data (PeanoNumber k) => Constructor k = Constructor
                        { constructorName      :: UpperDesignator
                        , constructorSemantics :: Annotation k
                        , constructorFields    :: [Field k] 
                        }
                        deriving (Eq, Ord, Show)

data (PeanoNumber k) => Field k = Field
                        { fieldName      :: LowerDesignator
                        , fieldSemantics :: Annotation k
                        , fieldType      :: Type k
                        }
                        deriving (Eq, Ord, Show)

data (PeanoNumber k) => Annotation k = Plain Text
                        deriving (Eq, Ord, Show)
 
data LatinAlphabet = A
                   | B
                   | C
                   | D
                   | E
                   | F
                   | G
                   | H
                   | I
                   | J
                   | K
                   | L
                   | M
                   | N
                   | O
                   | P
                   | Q
                   | R
                   | S
                   | T
                   | U
                   | V
                   | W
                   | X
                   | Y
                   | Z
                   deriving (Eq, Ord, Enum, Show)

data DecimalAlphabet = Zero
                     | One
                     | Two
                     | Three
                     | Four
                     | Five
                     | Six
                     | Seven
                     | Eight
                     | Nine
                     deriving (Eq, Ord, Enum, Show)

data (PeanoNumber a) => Binding a b c = Bind { associated :: b, bound :: (Binding (Succ a) b c) }
                                      | Expression { expression :: c a }

data Symbol = Lower   LatinAlphabet
            | Upper   LatinAlphabet
            | Decimal DecimalAlphabet
            | Underscore
            | Prime
            deriving (Eq, Ord, Show)

data LowerDesignator = LowerDesignator LatinAlphabet [Symbol] deriving (Eq, Ord)
data UpperDesignator = UpperDesignator LatinAlphabet [Symbol] deriving (Eq, Ord)

show' :: [Symbol] -> String
show' []              = []
show' (Underscore:xs) = '_':(show' xs)
show' (Prime     :xs) = '\'':(show' xs)
show' (Lower a   :xs) = (chr $ (fromEnum a)+97):(show' xs)
show' (Upper a   :xs) = (chr $ (fromEnum a)+65):(show' xs)
show' (Decimal a :xs) = (chr $ (fromEnum a)+48):(show' xs)

data Namespace = Namespace {
                             nstypes      :: S.Set UUID
                            ,nsclasses    :: S.Set UUID
                            ,subspaces    :: M.Map UpperDesignator Namespace
                           } deriving (Eq, Show)
                           

instance Show LowerDesignator where
  show (LowerDesignator a xs) = (chr $ (fromEnum a)+97):(show' xs)

instance Show UpperDesignator where
  show (UpperDesignator a xs) = (chr $ (fromEnum a)+65):(show' xs)


instance IsString LowerDesignator where
  fromString []     = error "LowerDesignator must consist of at least one lower letter."
  fromString (x:xs) | i < 97 || i > 122 = error $ "Character '"++(show x)++"' is not a lowercase letter."
                    | otherwise         = LowerDesignator (toEnum $ i-97) (fromString xs)  
                    where 
                      i = fromEnum x

instance IsString UpperDesignator where
  fromString []     = error "UpperDesignator must consist of at least one upper letter."
  fromString (x:xs) | i < 65 || i > 90  = error $ "Character '"++(show x)++"' is not an upperrcase letter."
                    | otherwise         = UpperDesignator (toEnum $ i-65) (fromString xs)  
                    where 
                      i = fromEnum x

instance IsString [Symbol] where
  fromString []        = []
  fromString ('_':xs)  = Underscore:(fromString xs) 
  fromString ('\'':xs) = Prime:(fromString xs)
  fromString (x  :xs)  | i >= 48 && i <= 57  = (Decimal $ toEnum $ i-48):(fromString xs)
                       | i >= 65 && i <= 90  = (Upper   $ toEnum $ i-65):(fromString xs)
                       | i >= 97 && i <= 122 = (Lower   $ toEnum $ i-97):(fromString xs)
                       | otherwise           = error $ "Character '"++(show x)++"' is not allowed here.'"
                       where i = fromEnum x

