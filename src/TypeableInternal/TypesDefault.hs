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


listof x =  Application (DataType "0ba85f3f10099c75d4b696d0cf944e09") (DataType x)
list   x =  Application (DataType "0ba85f3f10099c75d4b696d0cf944e09") x

--

defaultType_ :: (PeanoNumber a) => TypeDefinition a
defaultType_  = TypeDefinition {
                 identifier   = undefined
               , antecedent   = Nothing
               , created      = Time 384573
               , modified     = Time 345387
               , author       = Person
               , maintainer   = Person
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
