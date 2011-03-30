{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
{-# OPTIONS -XExistentialQuantification -XNoMonomorphismRestriction #-}
module Typeable.Internal.TypesDefault where

import Typeable.T9e2e1e478e094a8abe5507f8574ac91f --Succ
import Typeable.T421496848904471ea3197f25e2a02b72 --Zero
import Typeable.T0174bd2264004820bfe34e211cb35a7d --DataType
import Typeable.T1660b01f08dc4aedbe4c0941584541cb --Kind
import qualified Typeable.T3819884685d34bf19b3469304e15983d as Person

import qualified Data.Set as S
import qualified Data.Map as M

import Typeable.Internal.InternalTypeDefs

list     x =   Application (DataType "0ba85f3f10099c75d4b696d0cf944e09") x
set      x =   Application (DataType "7af30cce93724981a16a80f3f193dc33") x
maybe    x =   Application (DataType "f8f49ef6bbe874a42926fa23d5b3bc19") x
function x y = Application (Application (DataType "50eae3e85d2d42c88754b026cc360981") x) y

v0  :: a Zero -> Binding Zero Kind a
v0 x = Expression x

v1  :: a (Succ Zero) -> Binding Zero Kind a
v1 x = Bind KindStar (Expression x) 

v2  :: a (Succ (Succ Zero)) -> Binding Zero Kind a
v2 x = Bind KindStar (Bind KindStar (Expression x)) 

v3  :: a (Succ (Succ (Succ Zero))) -> Binding Zero Kind a
v3 x = Bind KindStar (Bind KindStar (Bind KindStar (Expression x))) 

v1'  :: Kind -> a (Succ Zero) -> Binding Zero Kind a
v1' a x = Bind a (Expression x) 

v2'  :: Kind -> Kind -> a (Succ (Succ Zero)) -> Binding Zero Kind a
v2' a b x = Bind a (Bind b (Expression x)) 

v3'  :: Kind -> Kind -> Kind -> a (Succ (Succ (Succ Zero))) -> Binding Zero Kind a
v3' a b c x = Bind a (Bind b (Bind c (Expression x))) 


defaultPerson = Person.Person { Person.name = "typeable.org", Person.contacts = S.empty }

personLars     = defaultPerson { Person.name = "Lars Petersen" }
personMikael   = defaultPerson { Person.name = "Mikael Voss" }
personClemens  = defaultPerson { Person.name = "Clemens Kornd\x00F6rfer" }
personStefan   = defaultPerson { Person.name = "Prof. Dr. Stefan Evert" }


