{-# OPTIONS -XEmptyDataDecls -XDeriveDataTypeable -XStandaloneDeriving -XFlexibleInstances -XOverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module TypeableInternal.InternalTypeDefs where

import Typeable.Cc6ebaa9f4cdc4068894d1ffaef5a7a83 -- PeanoNumber
import Typeable.T606f253533d3420da3465afae341d598 -- Time
import Typeable.Tc1b1f6c722c2436fab3180146520814e -- UTC
import Typeable.T9e2e1e478e094a8abe5507f8574ac91f -- Succ

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
import Data.Time.Calendar
import Network.URL
import Data.Char
import Prelude as P

import Data.Typeable
import Data.Data

import qualified Data.Map as M

type List a = [a]

var2String :: (PeanoNumber k) => k -> String
var2String x = [toEnum (97 + fromEnum x)]

instance IsString UUID where
  fromString = UUID . fromInteger . fst . P.head . readHex . (P.filter isHexDigit)

-- Wunderfein. Hierein könnte man sogar noch automatische Ersetzung von URLs etc. machen
instance PeanoNumber k => IsString (Annotation k) where
  fromString [] = Block M.empty
  fromString xs = Block (M.singleton ENG [Plain (fromString xs)])

data UUID       = UUID Word128 deriving (Eq, Ord)

instance Show UUID where
  show (UUID x) = f $ showHex (fromIntegral x :: Integer) ""
                  where
                    f xs | P.length xs < 32 = f ('0':xs)
                         | otherwise        = xs

type DateTime   = Int     

data Void

data Person = Person { personName :: String
                     } deriving (Eq, Ord, Show)

instance Eq Void where
  (==) = undefined

instance Ord Void where
  compare = undefined

instance Show Void where
  show = undefined

data (PeanoNumber k) => Type k = DataType      { typeRef ::  UUID }               -- entweder extern der Ordnung k
                               | DependentType  UUID               -- oder gebundene Variable
                               | Variable       k                  -- oder freie Variable der Ordnung k
                               | Application    (Type k) (Type k)  -- oder Typkonstruktor höherer Ordnung wird angewandt
                               | Quantification (Set (Constraint (Succ k))) (Type (Succ k))
                               deriving (Eq, Ord, Show)

data (PeanoNumber k) => Constraint k = Constraint
                                       { constraintClass :: UUID
                                       , constraintVars  :: [Type k]
                                       }
                                       deriving (Eq, Ord, Show)

data (PeanoNumber k) => TypeDefinition k = TypeDefinition
                        { identifier      :: UUID
                        , antecedent      :: Maybe UUID
                        , created         :: Time UTC
                        , modified        :: Time UTC
                        , author          :: Maybe Person
                        , maintainer      :: Person
                        , name            :: UpperDesignator
                        , semantics       :: Annotation k
                        , variables       :: Map k (Annotation k)
                        , constraints     :: Set (Constraint k)
                        , constructors    :: Maybe [Constructor k]
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

data ISO_639_2B = ENG deriving (Eq, Ord, Show)

type Language a   = Map ISO_639_2B a 

data (PeanoNumber k) => Annotation k   = Block          (Language [Inline k])
                                       | IndentList     [Annotation k]
                                       | BulletList     [Annotation k]
                                       | IndexedList    [Annotation k]
                                       | TitledList     [(Language Text, Annotation k)]
                                       deriving (Eq, Ord, Show)

data (PeanoNumber k) => Inline k       = Plain          Text
                                       | Emph           Text
                                       | Strong         Text
                                       | Subscript      Text
                                       | Superscript    Text
                                       | Monospace      Text
                                       | URL            URL 
                                       | Norm           Norm
                                       | Type           (Type k)
                                       | Class          UUID
                                       deriving (Eq, Ord, Show)

data Norm        = RFC  Word
                 | ISO  Word
                 | IEC  Word
                 | DIN  Word
                 | ECMA Word
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


data Symbol = Lower   LatinAlphabet
            | Upper   LatinAlphabet
            | Decimal DecimalAlphabet
            | Underscore
            deriving (Eq, Ord, Show)

data LowerDesignator = LowerDesignator LatinAlphabet [Symbol] deriving (Eq, Ord)
data UpperDesignator = UpperDesignator LatinAlphabet [Symbol] deriving (Eq, Ord)

show' :: [Symbol] -> String
show' []              = []
show' (Underscore:xs) = '_':(show' xs)
show' (Lower a   :xs) = (chr $ (fromEnum a)+97):(show' xs)
show' (Upper a   :xs) = (chr $ (fromEnum a)+65):(show' xs)
show' (Decimal a :xs) = (chr $ (fromEnum a)+48):(show' xs)



data Namespace = Namespace {
                             nstypes      :: S.Set UUID
                            ,nsclasses    :: S.Set UUID
                            ,subspaces  :: M.Map UpperDesignator Namespace
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
  fromString (x:xs) | i < 65 || i > 90  = error $ "Character '"++(show x)++"' is not a upperrcase letter."
                    | otherwise         = UpperDesignator (toEnum $ i-65) (fromString xs)  
                    where 
                      i = fromEnum x

instance IsString [Symbol] where
  fromString []       = []
  fromString ('_':xs) = Underscore:(fromString xs) 
  fromString (x  :xs) | i >= 48 && i <= 57  = (Decimal $ toEnum $ i-48):(fromString xs)
                      | i >= 65 && i <= 90  = (Upper   $ toEnum $ i-65):(fromString xs)
                      | i >= 97 && i <= 122 = (Lower   $ toEnum $ i-97):(fromString xs)
                      | otherwise           = error $ "Character '"++(show x)++"' is not allowed here.'"
                      where i = fromEnum x

--

{--
data StructuredText a = Paragraph   { paragraph   :: Map Language (List (TextElement a)) }
                      | IndentList  { indentList  :: List (StructuredText a) }
                      | BulletList  { bulletList  :: List (StructuredText a) }
                      | IndexedList { indexedList :: List (StructuredText a) }
                      | TitledList  { titledList  :: List (Tuple (Map Language T.Text) (StructuredText a)) }

data TextElement a = Text        { text      :: T.Text
                                 , weight    :: ()
                                 , italic    :: Bool
                                 , monospace :: Bool
                                 , cancelled :: Bool
                                 }
                   | Math        { tex :: T.Text }
                   | Hyperlink   { url :: T.Text }
                   | Extension   { ext :: a }
                   --}
