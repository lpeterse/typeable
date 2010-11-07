{-# OPTIONS -XFlexibleInstances #-}
module Typeable.T606f253533d3420da3465afae341d598 where

import Typeable.Tc1b1f6c722c2436fab3180146520814e -- UTC 

import Data.Ratio
import Data.Time.Calendar
import Data.Time.Clock
import Data.Time
import System.Locale

data Time a = Time { seconds :: Rational } deriving (Eq, Ord)

instance Show (Time UTC) where
  show (Time x) = formatTime defaultTimeLocale "%c" $ UTCTime (ModifiedJulianDay (fromIntegral d)) (secondsToDiffTime (fromIntegral s)) 
                where
                  y = (numerator x `div` denominator x)+1297728000
                  (d,s) = y `quotRem` (3600*24)

