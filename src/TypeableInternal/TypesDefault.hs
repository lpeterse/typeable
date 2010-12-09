{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -XExistentialQuantification #-}
module TypeableInternal.TypesDefault where

import TypeableInternal.InternalTypeDefs
import Typeable.Cc6ebaa9f4cdc4068894d1ffaef5a7a83
import Typeable.T421496848904471ea3197f25e2a02b72
import Typeable.T9e2e1e478e094a8abe5507f8574ac91f

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

defaultClass :: ClassDefinition (Succ Zero)
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
                , classMethods       = M.empty
                }

defaultType_ :: (PeanoNumber a) => TypeDefinition a
defaultType_  = TypeDefinition {
                 identifier   = undefined
               , antecedent   = Nothing
               , created      = Time 3499718400
               , modified     = Time 3499718400
               , author       = Nothing
               , maintainer   = defaultPerson
               , name         = undefined
               , semantics    = ""
               , variables    = M.empty
               , constraints  = S.empty
               , constructors = Just []
               }

defaultType :: TypeDefinition Zero
defaultType = defaultType_
               
defaultType' :: TypeDefinition (Succ Zero)    
defaultType' = defaultType_

defaultType'' :: TypeDefinition (Succ  (Succ Zero))
defaultType'' = defaultType_

defaultType''' :: TypeDefinition (Succ (Succ (Succ Zero)))           
defaultType''' = defaultType_



defaultConstructor_ :: (PeanoNumber a) => Constructor a
defaultConstructor_  = Constructor {
                         constructorName       = undefined
                       , constructorSemantics  = ""
                       , constructorFields     = []
                       }

defaultConstructor :: Constructor Zero
defaultConstructor = defaultConstructor_

defaultConstructor' :: Constructor (Succ Zero)
defaultConstructor' = defaultConstructor_

defaultConstructor'' :: Constructor (Succ (Succ Zero))
defaultConstructor'' = defaultConstructor_

defaultConstructor''' :: Constructor (Succ (Succ (Succ Zero)))
defaultConstructor''' = defaultConstructor_



defaultField_      :: (PeanoNumber a) => Field a
defaultField_       = Field {
                        fieldName             = undefined
                      , fieldSemantics        = ""
                      , fieldType             = undefined
                      }

defaultField  :: Field Zero
defaultField   = defaultField_

defaultField' :: Field (Succ Zero)
defaultField' = defaultField_

defaultField'' :: Field (Succ (Succ Zero))
defaultField'' = defaultField_

defaultField''' :: Field (Succ (Succ (Succ Zero)))
defaultField''' = defaultField_
