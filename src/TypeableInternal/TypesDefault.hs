{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
{-# OPTIONS -XExistentialQuantification -XNoMonomorphismRestriction #-}
module TypeableInternal.TypesDefault where

import TypeableInternal.InternalTypeDefs
import Typeable.T421496848904471ea3197f25e2a02b72
import Typeable.T606f253533d3420da3465afae341d598

import qualified Data.Set as S
import qualified Data.Map as M

import Data.Ratio

list     x =   Application (DataType "0ba85f3f10099c75d4b696d0cf944e09") x
set      x =   Application (DataType "7af30cce93724981a16a80f3f193dc33") x
maybe    x =   Application (DataType "f8f49ef6bbe874a42926fa23d5b3bc19") x
function x y = Application (Application (DataType "50eae3e85d2d42c88754b026cc360981") x) y

--

defaultPerson = Person { personName = "typeable.org" }

personLars     = defaultPerson { personName = "Lars Petersen" }
personMikael   = defaultPerson { personName = "Mikael Voss" }
personClemens  = defaultPerson { personName = "Clemens Kornd\x00F6rfer" }
personStefan   = defaultPerson { personName = "Prof. Dr. Stefan Evert" }

defaultClass :: ClassDefinition (Application Concrete Concrete)
defaultClass  = ClassDefinition
                { classIdentifier    = undefined
                , classAntecedent    = Nothing
                , classCreated       = Time 3499718400
                , classModified      = Time 3499718400
                , classAuthor        = Nothing
                , classMaintainer    = defaultPerson
                , className          = undefined
                , classSemantics     = ""
                , classVariables     = M.empty
                , classConstraints   = S.empty
                , classMethods       = []
                }


