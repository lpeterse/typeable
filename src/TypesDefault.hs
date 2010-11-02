{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -XExistentialQuantification #-}
module TypesDefault where

--import Typedoc
import InternalTypeDefs

import qualified Data.Set as S
import qualified Data.Map as M


listof x =  Reduction (Reference "0ba85f3f10099c75d4b696d0cf944e09") (Reference x)
list   x =  Reduction (Reference "0ba85f3f10099c75d4b696d0cf944e09") x

--

defaultType_ :: (Kind a) => TypeDefinition a
defaultType_  = TypeDefinition {
                 identifier   = undefined
               , antecedent   = Nothing
               , created      = 0
               , modified     = 0
               , author       = Person
               , maintainer   = Person
               , name         = undefined
               , semantics    = ""
               , variables    = M.empty
               , constraints  = S.empty
               , constructors = Just []
               }

defaultType :: TypeDefinition Concrete
defaultType = defaultType_
 
               
defaultType' :: TypeDefinition (Abstraction Concrete)    
defaultType' = defaultType_

defaultType'' :: TypeDefinition (Abstraction  (Abstraction Concrete))
defaultType'' = defaultType_

defaultType''' :: TypeDefinition (Abstraction (Abstraction (Abstraction Concrete)))           
defaultType''' = defaultType_



defaultConstructor_ :: (Kind a) => Constructor a
defaultConstructor_  = Constructor {
                         constructorName       = undefined
                       , constructorSemantics  = ""
                       , constructorFields     = []
                       }

defaultConstructor :: Constructor Concrete
defaultConstructor = defaultConstructor_

defaultConstructor' :: Constructor (Abstraction Concrete)
defaultConstructor' = defaultConstructor_

defaultConstructor'' :: Constructor (Abstraction (Abstraction Concrete))
defaultConstructor'' = defaultConstructor_

defaultConstructor''' :: Constructor (Abstraction (Abstraction (Abstraction Concrete)))
defaultConstructor''' = defaultConstructor_



defaultField_      :: (Kind a) => Field a
defaultField_       = Field {
                        fieldName             = undefined
                      , fieldSemantics        = ""
                      , fieldType             = undefined
                      }

defaultField  :: Field Concrete
defaultField   = defaultField_

defaultField' :: Field (Abstraction Concrete)
defaultField' = defaultField_

defaultField'' :: Field (Abstraction (Abstraction Concrete))
defaultField'' = defaultField_

defaultField''' :: Field (Abstraction (Abstraction (Abstraction Concrete)))
defaultField''' = defaultField_
