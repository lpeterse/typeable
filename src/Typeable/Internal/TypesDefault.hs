{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
{-# OPTIONS -XExistentialQuantification -XNoMonomorphismRestriction #-}
module Typeable.Internal.TypesDefault where

import Typeable.Internal.InternalTypeDefs

import qualified Data.Set as S
import qualified Data.Map as M
import Data.Ratio

list     x =   Application (DataType "0ba85f3f10099c75d4b696d0cf944e09") x
set      x =   Application (DataType "7af30cce93724981a16a80f3f193dc33") x
maybe    x =   Application (DataType "f8f49ef6bbe874a42926fa23d5b3bc19") x
function x y = Application (Application (DataType "50eae3e85d2d42c88754b026cc360981") x) y

v0  :: a Zero -> Binding Zero Kind' a
v0 x = Expression x

v1  :: a (Succ Zero) -> Binding Zero Kind' a
v1 x = Bind Concrete' (Expression x) 

v2  :: a (Succ (Succ Zero)) -> Binding Zero Kind' a
v2 x = Bind Concrete' (Bind Concrete' (Expression x)) 

v3  :: a (Succ (Succ (Succ Zero))) -> Binding Zero Kind' a
v3 x = Bind Concrete' (Bind Concrete' (Bind Concrete' (Expression x))) 

v1'  :: Kind' -> a (Succ Zero) -> Binding Zero Kind' a
v1' a x = Bind a (Expression x) 

v2'  :: Kind' -> Kind' -> a (Succ (Succ Zero)) -> Binding Zero Kind' a
v2' a b x = Bind a (Bind b (Expression x)) 

v3'  :: Kind' -> Kind' -> Kind' -> a (Succ (Succ (Succ Zero))) -> Binding Zero Kind' a
v3' a b c x = Bind a (Bind b (Bind c (Expression x))) 

defaultPerson = Person { personName = "typeable.org" }

personLars     = defaultPerson { personName = "Lars Petersen" }
personMikael   = defaultPerson { personName = "Mikael Voss" }
personClemens  = defaultPerson { personName = "Clemens Kornd\x00F6rfer" }
personStefan   = defaultPerson { personName = "Prof. Dr. Stefan Evert" }


