{-# OPTIONS -XNoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module TypeableInternal.Types where

import Prelude hiding (maybe)

import Typeable.T421496848904471ea3197f25e2a02b72

import TypeableInternal.TypesDefault
import TypeableInternal.InternalTypeDefs
import qualified Data.Set as S
import qualified Data.Map as M
import Data.Ratio
import Typeable.T606f253533d3420da3465afae341d598

defaultType :: forall a. Kind a => TypeDefinition a
defaultType  = TypeDefinition 
               { identifier   = undefined
               , antecedent   = Nothing
               , created      = Time 3499718400
               , modified     = Time 3499718400
               , author       = Nothing
               , maintainer   = defaultPerson
               , name         = undefined
               , semantics    = ""        --  :: Annotation a
               , variables    = M.empty     
               , constraints  = S.empty   --  :: S.Set (Constraint a)
               , constructors = Just []   --  :: Maybe [Constructor a]
               }


defaultConstructor :: forall a. Kind a => Constructor a
defaultConstructor  = Constructor {
                        constructorName       = undefined
                      , constructorSemantics  = "" -- :: Annotation a
                      , constructorFields     = [] -- :: [Field a]
                      }

defaultField       :: forall a. Kind a => Field a
defaultField        = Field {
                        fieldName             = undefined 
                      , fieldSemantics        = ""       --  :: Annotation a
                      , fieldType             = undefined -- :: Type a
                      }

-----
-- type-definitions (provisoric for as long as the binary format is not yet finalised)
-----


t1      :: TypeDefinition Concrete
t1       = defaultType {
             identifier   = "0219c59f732a8ef507215fbdb4cceacd"
           , name         = "Bool"
           , semantics    = "Boolean truth value."
           , constructors = Just [
                                   defaultConstructor { constructorName = "False" }
                                 , defaultConstructor { constructorName = "True"  }
                                 ]
          }

t56      :: TypeDefinition Concrete
t56      = defaultType {
             identifier   = "42149684-8904-471e-a319-7f25e2a02b72"
           , author       = Just personLars
           , name         = "Zero"
           , semantics    = "The typelevel number zero. Interpreted as a set it is the empty set. It therefore has no instances."
           , constructors = Just []
           }

t57 :: TypeDefinition (Application Concrete Concrete) 
t57      = defaultType {
             identifier   = "9e2e1e47-8e09-4a8a-be55-07f8574ac91f"
           , author       = Just personLars
           , name         = "Succ"
           , semantics    = "Counting in the peano sense. Interpreted as a set it is the set that contains n ordinal numbers."
           , constraints  = S.fromList [Constraint "c6ebaa9f-4cdc-4068-894d-1ffaef5a7a83" [Variable First]]
           , constructors = Just [
                                   defaultConstructor { constructorName   = "First" }
                                 , defaultConstructor { constructorName   = "Next"
                                                       , constructorFields = [ defaultField
                                                                               { fieldName = "previous"
                                                                               , fieldType = Variable First
                                                                               }
                                                                             ]
                                                       }
                                 ]
           }

t93 :: TypeDefinition Concrete 
t93      = defaultType {
             identifier   = "f4b6d72c609d4003ba98917f8c56a678"
           , author       = Nothing
           , name         = "Ordering"
           , semantics    = ""
           , constructors = Just [
                                   defaultConstructor { constructorName   = "LT", constructorSemantics = "Less"}
                                 , defaultConstructor { constructorName   = "EQ", constructorSemantics = "Equal"}
                                 , defaultConstructor { constructorName   = "GT", constructorSemantics = "Greater"}
                                 ]
           }

t94 :: TypeDefinition (Application Concrete Concrete) 
t94      = defaultType {
             identifier   = "2c62454c586f4bdea5e2b17e432db245"
           , author       = Just personLars
           , name         = "Extension"
           , semantics    = "An extension for making it possible to reference types, classes and functions within structured text."
           , constructors = Just [
                                   defaultConstructor { constructorName   = "Type"                 }
                                 , defaultConstructor { constructorName   = "TypeConstructor"      }
                                 , defaultConstructor { constructorName   = "TypeConstructorField" }
                                 , defaultConstructor { constructorName   = "Class"                }
                                 , defaultConstructor { constructorName   = "ClassMethod"          }
                                 , defaultConstructor { constructorName   = "Function"             }
                                 , defaultConstructor { constructorName   = "Constraint"           }
                                 , defaultConstructor { constructorName   = "Expression"           }
                                 ]
           }



t58     :: TypeDefinition (Application (Application Concrete Concrete) (Application Concrete Concrete))
t58      = defaultType {
             identifier   = "9035333f-c91c-42f8-ab8f-ba4c3f256a7b"
           , author       = Just personLars
           , name         = "FOL"
           , semantics    = "Formulas of the first order predicate logic. The structure is limited to the necessary: NAND. Other conjunctors are seen as a shorthand notation for: ..."
           , constraints  = S.fromList [ Constraint "0d864b18-19bd-4230-905b-bad04a4c195e" [Variable First]
                                       , Constraint "c6ebaa9f-4cdc-4068-894d-1ffaef5a7a83" [Variable (Next First)]
                                       ]
           , constructors = Just [
                                   defaultConstructor { constructorName   = "True" }
                                 , defaultConstructor { constructorName   = "Negation"
                                                        , constructorFields = [ defaultField
                                                                                { fieldName = "negated"
                                                                                , fieldType = Application
                                                                                                (Application
                                                                                                  (DataType "9035333f-c91c-42f8-ab8f-ba4c3f256a7b")
                                                                                                  (Variable First)
                                                                                                )
                                                                                                (Variable (Next First))
                                                                                }
                                                                              ]
                                                        }
                                 , defaultConstructor { constructorName   = "And"
                                                        , constructorFields = [ defaultField
                                                                                { fieldName = "left"
                                                                                , fieldType = Application
                                                                                                (Application
                                                                                                  (DataType "9035333f-c91c-42f8-ab8f-ba4c3f256a7b")
                                                                                                  (Variable First)
                                                                                                )
                                                                                                (Variable (Next First))
                                                                                }
                                                                              , defaultField
                                                                                { fieldName = "right"
                                                                                , fieldType = Application
                                                                                                (Application
                                                                                                  (DataType "9035333f-c91c-42f8-ab8f-ba4c3f256a7b")
                                                                                                  (Variable First)
                                                                                                )
                                                                                                (Variable (Next First))
                                                                                }
                                                                              ]
                                                        }
                                 , defaultConstructor { constructorName   = "Predicate"
                                                        , constructorFields = [ defaultField
                                                                                { fieldName = "predicate"
                                                                                , fieldType = Application
                                                                                                (Variable First)
                                                                                                (Variable (Next First))
                                                                                }
                                                                              ]
                                                        }
                                 , defaultConstructor { constructorName   = "Forall"
                                                        , constructorFields = [ defaultField
                                                                                { fieldName = "universal"
                                                                                , fieldType = Application
                                                                                                (Application
                                                                                                  (DataType "9035333f-c91c-42f8-ab8f-ba4c3f256a7b")
                                                                                                  (Variable First)
                                                                                                )
                                                                                                (Application 
                                                                                                  (DataType "9e2e1e47-8e09-4a8a-be55-07f8574ac91f")
                                                                                                  (Variable (Next First))
                                                                                                )
                                                                                }
                                                                              ]
                                                        }
                                 ]
          }


t86     :: TypeDefinition (Application Concrete (Application Concrete Concrete))
t86      = defaultType {
             identifier   = "50eae3e8-5d2d-42c8-8754-b026cc360981"
           , author       = Nothing
           , name         = "Function"
           , semantics    = "A function: a total mapping from the domain $a to the domain $b."
           , constructors = Nothing
          }

t87     :: TypeDefinition (Application Concrete Concrete)
t87      = defaultType {
             identifier   = "e590e9ce-9cea-4dfe-86a4-13e9270dd1c2"
           , author       = Nothing
           , name         = "Method"
           , semantics    = "A class method's name, signature and semantic."
           , constructors = Just [
                                   defaultConstructor
                                   { constructorName   = "Method"
                                   , constructorFields = [
                                                           defaultField
                                                           { fieldName = "name"
                                                           , fieldType = DataType "9790ade9814a4aaca5eaa80c3e47685d"
                                                           }
                                                         , defaultField
                                                           { fieldName = "signature"
                                                           , fieldType = Application (DataType "0174bd2264004820bfe34e211cb35a7d") (Variable First)
                                                           }
                                                         , defaultField
                                                           { fieldName = "semantics"
                                                           , fieldType = Application
                                                                           (DataType "b0221a43-509e-4edd-b062-101bfd794bc4")
                                                                           (Application
                                                                              (DataType "2c62454c586f4bdea5e2b17e432db245")
                                                                              (Variable First)
                                                                           )
                                                           }
                                                         ]
                                   }
                                 ]
          }



t52     :: TypeDefinition Concrete 
t52      = defaultType {
             identifier   = "2dbb6df8-73ad-4e4b-aeb8-2172074ed042"
           , author       = Just personLars
           , name         = "Gender"
           , semantics    = ""
           , constructors = Just [
                                   defaultConstructor { constructorName = "Male"    }
                                 , defaultConstructor { constructorName = "Female"  }
                                 ]
          }

t53     :: TypeDefinition Concrete 
t53      = defaultType {
             identifier   = "a0bbed72-1166-4a31-9e09-dc1c0f97bbd6"
           , author       = Just personLars
           , name         = "Casus"
           , semantics    = ""
           , constructors = Just [
                                   defaultConstructor { constructorName = "Nominativus" }
                                 , defaultConstructor { constructorName = "Genetivus"   }
                                 , defaultConstructor { constructorName = "Dativus"     }
                                 , defaultConstructor { constructorName = "Accusativus" }
                                 , defaultConstructor { constructorName = "Ablativus"   }
                                 , defaultConstructor { constructorName = "Vocativus"   }
                                 ]
          }

t54     :: TypeDefinition Concrete 
t54      = defaultType {
             identifier   = "a384955f-99d4-401c-a54a-3f9c62b78d0a"
           , author       = Just personLars
           , name         = "Numerus"
           , semantics    = ""
           , constructors = Just [
                                   defaultConstructor { constructorName = "Singularis" }
                                 , defaultConstructor { constructorName = "Pluralis"   }
                                 ]
          }
 
t55     :: TypeDefinition Concrete 
t55      = defaultType {
             identifier   = "ce462e9d-f114-4a16-8188-6cd2619b5d1a"
           , author       = Just personLars
           , name         = "Genus"
           , semantics    = ""
           , constructors = Just [
                                   defaultConstructor { constructorName = "Masculinum" }
                                 , defaultConstructor { constructorName = "Femininum"  }
                                 , defaultConstructor { constructorName = "Neutrum"  }
                                 ]
          }
          
t49     :: TypeDefinition Concrete 
t49      = defaultType {
             identifier   = "3819884685d34bf19b3469304e15983d"
           , author       = Just personLars
           , name         = "Person"
           , semantics    = "A record of information for identifying someone."
           , constructors = Just [
                                   defaultConstructor
                                   { constructorName    = "Person"
                                   , constructorFields  = [
                                                            defaultField
                                                            { fieldName     = "name"
                                                            , fieldType     = DataType "4f7db06c439541658a09689d3e7dd909"
                                                            }
                                                          , defaultField
                                                            { fieldName     = "birthday"
                                                            , fieldType     = maybe $ DataType "b6831ec097f14b8eba74b1e486b4175d"
                                                            }
                                                          , defaultField
                                                            { fieldName     = "contacts"
                                                            , fieldType     = set $ DataType "53e0d483-a641-4425-9dce-752799d64305"
                                                            }
                                                          ]
                                   }
                                 ]
          }

type T96 = (Application Concrete (Application (Application Concrete Concrete) (Application (Application Concrete Concrete) Concrete)))


t96     :: TypeDefinition T96
t96      = defaultType {
             identifier   = "ce4219d18285469abe0b8a07bb2a1742"
           , author       = Just personLars
           , name         = "Quantification"
           , semantics    = "'Generates' a finite domain of variables that get associated with values of $b and passed over to $c. $a is the current domain."
           , constructors = Just [
                                   (defaultConstructor :: Constructor T96)
                                   { constructorName    = "AddVariable"
                                   , constructorFields  = [
                                                            (defaultField :: Field T96)
                                                            { fieldName = "associated"
                                                            , fieldSemantics = "A value associated with the bound variable. It can make use of former bound variables."
                                                            , fieldType = Application (Variable (Next First)) (Variable First)
                                                            }
                                                          , (defaultField :: Field T96)
                                                            { fieldName = "quantified"
                                                            , fieldSemantics = "Recursive nesting."
                                                            , fieldType = Application
                                                                            (Application
                                                                              (Application
                                                                                (DataType "ce4219d18285469abe0b8a07bb2a1742")
                                                                                (Application
                                                                                  (DataType "9e2e1e478e094a8abe5507f8574ac91f")
                                                                                  (Variable First)
                                                                                )
                                                                              )
                                                                              (Variable (Next First))
                                                                            )
                                                                            (Variable (Next (Next First)))
                                                            }
                                                          ]
                                   }
                                 , (defaultConstructor :: Constructor T96)
                                   { constructorName    = "Expression"
                                   , constructorSemantics = "The expression whose variables got bound."
                                   , constructorFields  = [
                                                            (defaultField :: Field T96)
                                                            { fieldName = "expression"
                                                            , fieldType = Application
                                                                            (Variable (Next (Next First)))
                                                                            (Variable First)
                                                            }
                                                          ]
                                   }
                                 ]
          }




t97     :: TypeDefinition Concrete 
t97      = defaultType {
             identifier   = "1660b01f08dc4aedbe4c0941584541cb"
           , author       = Just personLars
           , name         = "Kind"
           , semantics    = "Representation of a type's kind."
           , constructors = Just [
                                   defaultConstructor
                                   { constructorName    = "Type"
                                   , constructorFields  = []
                                   }
                                 , defaultConstructor
                                   { constructorName    = "Application"
                                   , constructorFields  = [
                                                            defaultField
                                                            { fieldName = "function"
                                                            , fieldType = DataType "1660b01f08dc4aedbe4c0941584541cb"
                                                            }
                                                          , defaultField
                                                            { fieldName = "argument"
                                                            , fieldType = DataType "1660b01f08dc4aedbe4c0941584541cb"
                                                            }
                                                          ]
                                   }
                                 ]
          }

t98     :: TypeDefinition (Application Concrete (Application Concrete Concrete)) 
t98      = defaultType {
             identifier   = "0c6cffdb-eb90-4da7-b054-b96e2529237c"
           , author       = Just personLars
           , name         = "Const"
           , semantics    = "The type-level equivalent to ->const."
           , constructors = Just [
                                   (defaultConstructor ::  Constructor (Application Concrete (Application Concrete Concrete))) 
                                   { constructorName    = "Const"
                                   , constructorFields  = [
                                                            (defaultField :: Field  (Application Concrete (Application Concrete Concrete))) 
                                                            { fieldName = "const"
                                                            , fieldType = Variable First
                                                            }
                                                          ]
                                   }
                                 ]
          }


t79     :: TypeDefinition Concrete 
t79      = defaultType {
             identifier   = "b586fd6a-e075-49c4-b641-465feb232a00"
           , author       = Just personLars
           , name         = "TeXMath"
           , semantics    = "According to John McFarlane's definition at https://github.com/jgm/texmath/blob/master/Text/TeXMath/Parser.hs."
           , constructors = Just [
                                   defaultConstructor
                                   { constructorName    = "Number"
                                   , constructorFields  = []
                                   }
                                 , defaultConstructor
                                   { constructorName    = "Grouped"
                                   , constructorFields  = []
                                   }
                                 , defaultConstructor
                                   { constructorName    = "Identifier"
                                   , constructorFields  = []
                                   }
                                 , defaultConstructor
                                   { constructorName    = "MathOperator"
                                   , constructorFields  = []
                                   }
                                 , defaultConstructor
                                   { constructorName    = "Symbol"
                                   , constructorFields  = []
                                   }
                                 , defaultConstructor
                                   { constructorName    = "Space"
                                   , constructorFields  = []
                                   }
                                 , defaultConstructor
                                   { constructorName    = "Sub"
                                   , constructorFields  = []
                                   }
                                 , defaultConstructor
                                   { constructorName    = "Super"
                                   , constructorFields  = []
                                   }
                                 , defaultConstructor
                                   { constructorName    = "Subsup"
                                   , constructorFields  = []
                                   }
                                 , defaultConstructor
                                   { constructorName    = "Over"
                                   , constructorFields  = []
                                   }
                                 , defaultConstructor
                                   { constructorName    = "Under"
                                   , constructorFields  = []
                                   }
                                 , defaultConstructor
                                   { constructorName    = "Underover"
                                   , constructorFields  = []
                                   }
                                 , defaultConstructor
                                   { constructorName    = "Up"
                                   , constructorFields  = []
                                   }
                                 , defaultConstructor
                                   { constructorName    = "Down"
                                   , constructorFields  = []
                                   }
                                 , defaultConstructor
                                   { constructorName    = "Downup"
                                   , constructorFields  = []
                                   }
                                 , defaultConstructor
                                   { constructorName    = "Unary"
                                   , constructorFields  = []
                                   }
                                 , defaultConstructor
                                   { constructorName    = "Scaled"
                                   , constructorFields  = []
                                   }
                                 , defaultConstructor
                                   { constructorName    = "Stretchy"
                                   , constructorFields  = []
                                   }
                                 , defaultConstructor
                                   { constructorName    = "Array"
                                   , constructorFields  = []
                                   }
                                 , defaultConstructor
                                   { constructorName    = "Text"
                                   , constructorFields  = []
                                   }
                                 ]
                                }
 

t48     :: TypeDefinition (Application Concrete Concrete) 
t48      = defaultType {
             identifier   = "9592f9fa4fae437a9e8d0917c14ff068"
           , author       = Just personLars
           , name         = "TextElement"
           , semantics    = "Used by -> StructuredText."
           , constructors = Just [
                                   defaultConstructor
                                   { constructorName    = "Text"
                                   , constructorFields  = [
                                                            (defaultField :: Field (Application Concrete Concrete))
                                                            { fieldName      = "text"
                                                            , fieldType      = DataType "4f7db06c439541658a09689d3e7dd909" :: Type (Application Concrete Concrete)
                                                            }
                                                          , (defaultField :: Field (Application Concrete Concrete))
                                                            { fieldName      = "bold"
                                                            , fieldSemantics = "States whether the text is to be printed bold."
                                                            , fieldType      = DataType "0219c59f732a8ef507215fbdb4cceacd"
                                                            }
                                                          , (defaultField :: Field (Application Concrete Concrete))
                                                            { fieldName      = "italic"
                                                            , fieldSemantics = "States whether the text is to be printed italic."
                                                            , fieldType      = DataType "0219c59f732a8ef507215fbdb4cceacd"
                                                            }
                                                          , (defaultField :: Field (Application Concrete Concrete))
                                                            { fieldName      = "monospace"
                                                            , fieldSemantics = "States whether the text is to be printed with fixed length characters."
                                                            , fieldType      = DataType "0219c59f732a8ef507215fbdb4cceacd"
                                                            }
                                                          , (defaultField :: Field (Application Concrete Concrete))
                                                            { fieldName      = "cancelled"
                                                            , fieldSemantics = "States whether the text is stroked out."
                                                            , fieldType      = DataType "0219c59f732a8ef507215fbdb4cceacd"
                                                            }
                                                          , (defaultField :: Field (Application Concrete Concrete))
                                                            { fieldName      = "reference"
                                                            , fieldType      = maybe $ DataType "e393b15b944c4b3597cd02b1be6d693b"
                                                            }
                                                         ]
                                   }
                                 , defaultConstructor 
                                   { constructorName   = "Math"
                                   , constructorFields = [
                                                           defaultField
                                                           { fieldName = "tex"
                                                           , fieldType = DataType "b586fd6a-e075-49c4-b641-465feb232a00"
                                                           }
                                                         ]
                                   }
                                 , defaultConstructor 
                                   { constructorName   = "Extension"
                                   , constructorFields = [
                                                           defaultField
                                                           { fieldName = "ext"
                                                           , fieldType = Variable First
                                                           }
                                                         ]
                                   }
                                 ]
          }
          
t45     :: TypeDefinition Concrete
t45      = defaultType {
             identifier   = "c1b1f6c7-22c2-436f-ab31-80146520814e"
           , author       = Just personMikael
           , name         = "UTC"
           , semantics    = "The UTC time standard."
           , constructors = Just [] 
           }
          
t46     :: TypeDefinition Concrete
t46      = defaultType {
             identifier   = "aaaa6ecd-826d-4604-801c-4fa962cc1446"
           , author       = Just personMikael
           , name         = "TAI"
           , semantics    = "The TAI time standard."
           , constructors = Just [] 
           }
          
t47     :: TypeDefinition (Application Concrete Concrete)
t47      = defaultType {
             identifier   = "2a94a7a8d4e049759d8dd546e72293ff"
           , name         = "Constraint"
           , semantics    = "A class constraint on variables given as $a."
           , constructors = Just [] 
           }
          
          
t2      :: TypeDefinition (Application Concrete Concrete)
t2       = defaultType {
             identifier   = "0ba85f3f10099c75d4b696d0cf944e09"
           , name         = "List"
           , semantics    = "This is the default type for listing something. The order of elements matters and elements may occur more than once."
           , constructors = Just [
                                   defaultConstructor
                                     { constructorName      = "Nil"
                                     , constructorSemantics = "The empty list. Terminates recursion."
                                     } 
                                 , (defaultConstructor :: Constructor (Application Concrete Concrete)) 
                                     { constructorName      = "Cons"
                                     , constructorSemantics = "This constructor prepends an element to a remaining list."
                                     , constructorFields    = [   (defaultField :: Field  (Application Concrete Concrete))
                                                                  { fieldName      = "head"
                                                                  , fieldSemantics = "element"
                                                                  , fieldType      = (Variable First)
                                                                  }
                                                               ,  (defaultField :: Field  (Application Concrete Concrete)) 
                                                                  { fieldName      = "tail"
                                                                  , fieldSemantics = "the remaining list"
                                                                  , fieldType      = Application 
                                                                                       (DataType "0ba85f3f10099c75d4b696d0cf944e09") 
                                                                                       (Variable First)  
                                                                  }
                                                              ]
                                     } 
                                 ]
                       }

t43     :: TypeDefinition (Application Concrete Concrete) 
t43       = defaultType {
             identifier   = "b0221a43-509e-4edd-b062-101bfd794bc4"
           , author       = Just personLars
           , name         = "StructuredText"
           , semantics    = "A Markup format."
           , constraints  = S.fromList [Constraint "edba1ef6-3e72-4b61-8256-9040555253a8" [Variable First]]
           , constructors = Just [ 
                                   defaultConstructor
                                   { constructorName   = "Paragraph"
                                   , constructorFields = [
                                                           defaultField
                                                           { fieldName   = "paragraph"
                                                           , fieldType   = Application
                                                                             (Application
                                                                               (DataType "43c6cd1333b04fc8a480668ecb24768e")
                                                                               (DataType "af20e1db8f0d414f90625b1521e41378")
                                                                             )
                                                                             (Application
                                                                               (DataType "0ba85f3f10099c75d4b696d0cf944e09")
                                                                               (Application
                                                                                 (DataType "9592f9fa-4fae-437a-9e8d-0917c14ff068")
                                                                                 (Variable First)
                                                                               )
                                                                             )
                                                           }
                                                         ]
                                   }
                                 , defaultConstructor
                                   { constructorName   = "IndentList"
                                   , constructorFields = [
                                                           defaultField
                                                           { fieldName   = "indentList"
                                                           , fieldType   = Application
                                                                            (DataType "0ba85f3f10099c75d4b696d0cf944e09")
                                                                            (Application
                                                                              (DataType "b0221a43509e4eddb062101bfd794bc4")
                                                                              (Variable First)
                                                                            )
                                                           }
                                                         ]
                                   }
                                 , defaultConstructor
                                   { constructorName   = "BulletList"
                                   , constructorFields = [
                                                           defaultField
                                                           { fieldName   = "bulletList"
                                                           , fieldType   = Application
                                                                            (DataType "0ba85f3f10099c75d4b696d0cf944e09")
                                                                            (Application
                                                                              (DataType "b0221a43509e4eddb062101bfd794bc4")
                                                                              (Variable First)
                                                                            )
                                                           }
                                                         ]
                                   }
                                 , defaultConstructor
                                   { constructorName   = "IndexedList"
                                   , constructorFields = [
                                                           defaultField
                                                           { fieldName   = "indexedList"
                                                           , fieldType   = Application
                                                                            (DataType "0ba85f3f10099c75d4b696d0cf944e09")
                                                                            (Application
                                                                              (DataType "b0221a43509e4eddb062101bfd794bc4")
                                                                              (Variable First)
                                                                            )
                                                           }
                                                         ]
                                   }
                                 , defaultConstructor
                                   { constructorName   = "TitledList"
                                   , constructorFields = [
                                                           defaultField
                                                           { fieldName   = "titledList"
                                                           , fieldType   = Application
                                                                             (DataType "0ba85f3f10099c75d4b696d0cf944e09") -- List
                                                                             (Application
                                                                               (Application
                                                                                 (DataType "34c13bdaac7d413ed735e64edcac7ff5") -- Tuple
                                                                                 (Application 
                                                                                   (Application
                                                                                     (DataType "43c6cd1333b04fc8a480668ecb24768e") -- Map
                                                                                     (DataType "af20e1db8f0d414f90625b1521e41378") -- Language
                                                                                   )
                                                                                   (DataType "4f7db06c439541658a09689d3e7dd909") -- Text
                                                                                 )
                                                                               )
                                                                               (Application
                                                                                 (DataType "b0221a43509e4eddb062101bfd794bc4") -- StructuredText
                                                                                 (Variable First)
                                                                               )
                                                                             )
                                                           }
                                                         ]
                                   }
                                 ]
          }

t44     :: TypeDefinition (Application Concrete Concrete) 
t44       = defaultType {
             identifier   = "37c8a341f0b34cc6bbbc9f2403f09be3"
           , author       = Just personLars
           , name         = "Constructor"
           , semantics    = "A value constructor."
           , constraints  = S.fromList [Constraint "c6ebaa9f4cdc4068894d1ffaef5a7a83" [Variable First]]
           , constructors = Just [ defaultConstructor 
                                   { constructorName      = "Constructor"
                                   , constructorFields    = [
                                                                  (defaultField :: Field  (Application Concrete Concrete))
                                                                  { fieldName      = "name"
                                                                  , fieldSemantics = ""
                                                                  , fieldType      = DataType "44d86fd3a506477ab88683d796e0d18b"  
                                                                  }
                                                               ,  (defaultField :: Field  (Application Concrete Concrete))
                                                                  { fieldName      = "semantics"
                                                                  , fieldSemantics = ""
                                                                  , fieldType      = Application 
                                                                                       (DataType "b0221a43-509e-4edd-b062-101bfd794bc4")
                                                                                       (Application
                                                                                         (DataType "2c62454c586f4bdea5e2b17e432db245")
                                                                                         (Variable First)
                                                                                       )
                                                                  }
                                                               ,  (defaultField :: Field  (Application Concrete Concrete)) 
                                                                  { fieldName      = "fields"
                                                                  , fieldSemantics = ""
                                                                  , fieldType      = list (Application (DataType "205895c8-d2df-475b-8d5e-ad5ee33d9f63") (Variable First))
                                                                  }
                                                               ]
                                   }
                                 ]
          }

t51     :: TypeDefinition (Application Concrete Concrete) 
t51      = defaultType {
             identifier   = "205895c8-d2df-475b-8d5e-ad5ee33d9f63"
           , author       = Just personLars
           , name         = "Field"
           , constraints  = S.fromList [Constraint "c6ebaa9f4cdc4068894d1ffaef5a7a83" [Variable First]]
           , semantics    = ""
           , constructors = Just [ defaultConstructor 
                                   { constructorName      = "Field"
                                   , constructorFields    = [
                                                                  (defaultField  :: Field  (Application Concrete Concrete))
                                                                  { fieldName      = "name"
                                                                  , fieldSemantics = ""
                                                                  , fieldType      = DataType "9790ade9814a4aaca5eaa80c3e47685d"  
                                                                  }
                                                               ,  (defaultField :: Field (Application Concrete Concrete))
                                                                  { fieldName      = "semantics"
                                                                  , fieldSemantics = ""
                                                                  , fieldType      = Application 
                                                                                       (DataType "b0221a43-509e-4edd-b062-101bfd794bc4")
                                                                                       (Application
                                                                                         (DataType "2c62454c586f4bdea5e2b17e432db245")
                                                                                         (Variable First)
                                                                                       )
                                                                  }
                                                               ,  (defaultField :: Field (Application Concrete Concrete))
                                                                  { fieldName      = "type"
                                                                  , fieldSemantics = ""
                                                                  , fieldType      = Application 
                                                                                       (DataType "0174bd2264004820bfe34e211cb35a7d") 
                                                                                       (Variable First)
                                                                  }
                                                               ]
                                   }
                                 ]
          }

t95     :: TypeDefinition (Application (Application Concrete Concrete) Concrete)
t95      = defaultType {
             identifier   = "451f847e1cb642d0b7c5dbdfa03f41b5"
           , author       = Just personLars
           , name         = "Definition"
           , semantics    = ""
           , constructors = Just [
                                   defaultConstructor 
                                     { constructorName      = "Definition"
                                     , constructorFields    = [   (defaultField  :: Field Concrete)
                                                                  { fieldName      = "identifier"
                                                                  , fieldSemantics = "This identifier is bound to the structure and semantics, not to an actual version of discription etc."
                                                                  , fieldType      = DataType "346674042a7248b4a94abff0726d0c43"
                                                                  }
                                                               ,  (defaultField  :: Field Concrete)
                                                                  { fieldName      = "antecedent"
                                                                  , fieldSemantics = "Note whether this is an improved version with changes to structure and/or semantics."
                                                                  , fieldType      = Application 
                                                                                       (DataType "f8f49ef6bbe874a42926fa23d5b3bc19") 
                                                                                       (DataType "346674042a7248b4a94abff0726d0c43")  
                                                                  }
                                                               ,  (defaultField  :: Field Concrete)
                                                                  { fieldName      = "name"
                                                                  , fieldSemantics = "A human readable designation. It doesn't need to be unique. In doubt choose a short one that already catches the semantics as good as possible."
                                                                  , fieldType      = DataType "44d86fd3a506477ab88683d796e0d18b"  
                                                                  }
                                                               ,  (defaultField  :: Field Concrete)
                                                                  { fieldName      = "creationTime"
                                                                  , fieldSemantics = "The date of creating structure and semantics."
                                                                  , fieldType      = Application 
                                                                                       (DataType "606f253533d3420da3465afae341d598")
                                                                                       (DataType "c1b1f6c7-22c2-436f-ab31-80146520814e")
                                                                  }
                                                               ,  (defaultField :: Field Concrete)
                                                                  { fieldName      = "modificationTime"
                                                                  , fieldSemantics = "The date of the last improvement/modification to the description."
                                                                  , fieldType      = Application 
                                                                                       (DataType "606f253533d3420da3465afae341d598")
                                                                                       (DataType "c1b1f6c7-22c2-436f-ab31-80146520814e")
                                                                  }
                                                               ,  (defaultField :: Field Concrete)
                                                                  { fieldName      = "author"
                                                                  , fieldSemantics = "The original author. Nothing denotes that it belongs to the public domain."
                                                                  , fieldType      = maybe $ DataType "38198846-85d3-4bf1-9b34-69304e15983d"  
                                                                  }
                                                               ,  (defaultField :: Field Concrete)
                                                                  { fieldName      = "maintainer"
                                                                  , fieldSemantics = "Who is responsible for changes/additions to the description etc.?"
                                                                  , fieldType      =  DataType "38198846-85d3-4bf1-9b34-69304e15983d"   
                                                                  }
                                                               ,  (defaultField :: Field Concrete)
                                                                  { fieldName      = "structure"
                                                                  , fieldSemantics = ""
                                                                  , fieldType      = Application
                                                                                       (Application
                                                                                         (Application
                                                                                           (DataType "ce4219d1-8285-469a-be0b-8a07bb2a1742") --Quantification
                                                                                           (DataType "421496848904471ea3197f25e2a02b72")     --Zero
                                                                                         )
                                                                                         (Application 
                                                                                           (DataType "0c6cffdb-eb90-4da7-b054-b96e2529237c")
                                                                                           (DataType "1660b01f-08dc-4aed-be4c-0941584541cb")
                                                                                         )
                                                                                       )
                                                                                       (Variable First)
                                                                  }
                                                           ]
                                     } 
                   ]
               }


t42     :: TypeDefinition (Application Concrete Concrete)
t42      = defaultType {
             identifier   = "3e815311-18e1-4888-be21-de7921b15bb5"
           , author       = Just personLars
           , name         = "Type"
           , semantics    = "This is the datatype the whole system relies on :-)"
           , constraints  = S.fromList [Constraint "c6ebaa9f4cdc4068894d1ffaef5a7a83" [Variable First]]
           , constructors = Just [
                                   defaultConstructor 
                                     { constructorName      = "Type"
                                     , constructorFields    = [
                                                                  (defaultField :: Field  (Application Concrete Concrete))
                                                                  { fieldName      = "semantics"
                                                                  , fieldSemantics = "The type's semantics in general. Details may be described in the constructors or fields."
                                                                  , fieldType      = Application 
                                                                                      (DataType "b0221a43-509e-4edd-b062-101bfd794bc4")
                                                                                      (Application
                                                                                        (DataType "2c62454c586f4bdea5e2b17e432db245")
                                                                                        (Variable First)
                                                                                      )
                                                                  }
                                                               ,  (defaultField :: Field  (Application Concrete Concrete))
                                                                  { fieldName      = "constraints"
                                                                  , fieldSemantics = "Constraints on the type's free variables."
                                                                  , fieldType      = Application
                                                                                       (DataType "7af30cce93724981a16a80f3f193dc33")
                                                                                       (Application
                                                                                         (DataType "2a94a7a8-d4e0-4975-9d8d-d546e72293ff")
                                                                                         (Variable First)
                                                                                       )
                                                                  }
                                                               ,  (defaultField :: Field  (Application Concrete Concrete))
                                                                  { fieldName      = "constructors"
                                                                  , fieldSemantics = "The type's value constructors. If it is nothing, this means the type is abstract. Otherwise the constructors are listed whereas the order is relevant. Note the semantic difference between just the empty list and nothing."
                                                                  , fieldType      = Application 
                                                                                       (DataType "f8f49ef6bbe874a42926fa23d5b3bc19")  
                                                                                       (list      (Application (DataType "37c8a341-f0b3-4cc6-bbbc-9f2403f09be3") (Variable First)))
                                                                  }
                                                           ]
                                     } 
                   ]
                       }

t80     :: TypeDefinition (Application Concrete Concrete)
t80      = defaultType {
             identifier   = "4e0b8f8e-a2b1-4522-8fa4-ec74b559bf6a"
           , author       = Just personLars
           , name         = "Class"
           , semantics    = ""
           , constructors = Just [
                                   defaultConstructor 
                                     { constructorName      = "Class"
                                     , constructorFields    = [   (defaultField ::  Field (Application Concrete Concrete))
                                                                  { fieldName      = "semantics"
                                                                  , fieldSemantics = "The classes' semantics in general."
                                                                  , fieldType      = Application 
                                                                                      (DataType "b0221a43-509e-4edd-b062-101bfd794bc4")
                                                                                      (Application
                                                                                        (DataType "2c62454c586f4bdea5e2b17e432db245")
                                                                                        (Variable First)
                                                                                      )
                                                                  }
                                                               ,  (defaultField :: Field  (Application Concrete Concrete))
                                                                  { fieldName      = "constraints"
                                                                  , fieldSemantics = "Constraints on the type's free variables."
                                                                  , fieldType      = Application
                                                                                       (DataType "7af30cce93724981a16a80f3f193dc33")
                                                                                       (Application
                                                                                         (DataType "2a94a7a8-d4e0-4975-9d8d-d546e72293ff")
                                                                                         (Variable First)
                                                                                       )
                                                                  }
                                                              ,   (defaultField :: Field  (Application Concrete Concrete))
                                                                  { fieldName      = "methods"
                                                                  , fieldSemantics = ""
                                                                  , fieldType      = list (Application
                                                                                         (DataType "e590e9ce9cea4dfe86a413e9270dd1c2")
                                                                                         (Variable First)
                                                                                       )
                                                                  }
                                                           ]
                                     } 
                                 ]
                       }


t85     :: TypeDefinition Concrete 
t85       = defaultType {
              identifier   = "53e0d483a64144259dce752799d64305"
            , author       = Just personLars
            , name         = "ContactInformation"
            , semantics    = ""
            , constructors = Just [
                                    (defaultConstructor :: Constructor Concrete)
                                    { constructorName      = "Email"
                                    , constructorSemantics = ""
                                    , constructorFields    = [
                                                               defaultField
                                                               { fieldName      = "email"
                                                               , fieldType      = Application
                                                                                    (DataType "1ea5eae4-7028-44f7-acbc-3c65b2a40093")
                                                                                    (DataType "ed098cc9-75df-4cd0-adb9-9a5b7dc48600")
                                                               }                                                               
                                                             ]
                                    }
                                  , (defaultConstructor :: Constructor Concrete)
                                    { constructorName      = "Phone"
                                    , constructorSemantics = ""
                                    , constructorFields    = [
                                                               defaultField
                                                               { fieldName      = "phone"
                                                               , fieldType      = Application
                                                                                    (DataType "1ea5eae4-7028-44f7-acbc-3c65b2a40093")
                                                                                    (DataType "f18ae792-e532-4a68-a16f-11ea5c61442a")
                                                               }                                                               
                                                             ]
                                    }
                                  , (defaultConstructor :: Constructor Concrete)
                                    { constructorName      = "Website"
                                    , constructorSemantics = ""
                                    , constructorFields    = [
                                                               defaultField
                                                               { fieldName      = "website"
                                                               , fieldType      = Application
                                                                                    (DataType "1ea5eae4-7028-44f7-acbc-3c65b2a40093")
                                                                                    (DataType "d847a61a-1a94-4723-ab4b-fcfb214bd8aa")
                                                               }                                                               
                                                             ]
                                    }
                                  ]
            }

t81     :: TypeDefinition Concrete 
t81       = defaultType {
              identifier   = "ed098cc9-75df-4cd0-adb9-9a5b7dc48600"
            , author       = Just personLars
            , name         = "Mailto"
            , semantics    = "The URI scheme 'mailto' according to RFC 2368."
            , constructors = Just []
            }
 
t82     :: TypeDefinition Concrete 
t82       = defaultType {
              identifier   = "f18ae792-e532-4a68-a16f-11ea5c61442a"
            , author       = Just personLars
            , name         = "Tel"
            , semantics    = "The URI scheme 'tel' according to RFC 3966."
            , constructors = Just []
            }
 
t83     :: TypeDefinition Concrete 
t83       = defaultType {
              identifier   = "d847a61a-1a94-4723-ab4b-fcfb214bd8aa"
            , author       = Just personLars
            , name         = "Http"
            , semantics    = "The URI scheme 'http' according to RFC 1738, RFC 2068, RFC 2616."
            , constructors = Just []
            }
 
t84     :: TypeDefinition (Application Concrete Concrete) 
t84       = defaultType
            { identifier   = "1ea5eae4-7028-44f7-acbc-3c65b2a40093"
            , antecedent   = Just  "e393b15b-944c-4b35-97cd-02b1be6d693b"
            , author       = Just personLars
            , name         = "UriByScheme"
            , semantics    = "Uniform Resource Identifier variable in the scheme."
            , constructors = Just [
                                   defaultConstructor 
                                    { constructorName = "Uri"
                                    , constructorFields = [
                                                            defaultField 
                                                            { fieldName = "hierarchy"
                                                            , fieldType = DataType "8068cbda-f35e-4618-a7e7-98c67ff9bee0"
                                                            }
                                                          , 
                                                            defaultField 
                                                            { fieldName = "query"
				  				  				  				  				  				  , fieldType = DataType "4f7db06c439541658a09689d3e7dd909"
			    			    			    			    			    			    }
                                                          , 
                                                            defaultField
                                                            { fieldName = "fragment"
                                                            , fieldType = DataType "4f7db06c439541658a09689d3e7dd909"
                                                            }
                                                          ]
                                    }
                                  ]
				  	} 			    			    			    			    			    
                                                            
t41     :: TypeDefinition (Application Concrete Concrete)
t41       = defaultType {
             identifier   = "0174bd22-6400-4820-bfe3-4e211cb35a7d"
           , author       = Just personLars
           , name         = "DataType"
           , semantics    = ""
           , constraints  = S.fromList [Constraint "c6ebaa9f4cdc4068894d1ffaef5a7a83" [Variable First]]
           , constructors = Just [
                                   (defaultConstructor  :: Constructor  (Application Concrete Concrete) )
                                   { constructorName      = "DataType"
                                   , constructorSemantics = "References another Datatype by -> UUID."
                                   , constructorFields    = [ (defaultField  :: Field (Application Concrete Concrete))
                                                              { fieldName      = "typeRef"
                                                              , fieldType      = DataType "346674042a7248b4a94abff0726d0c43"
                                                              }
                                                            ]
                                   } 
                                 , (defaultConstructor  :: Constructor  (Application Concrete Concrete) )
                                   { constructorName      = "Variable"
                                   , constructorSemantics = "Bound variable. You supply the set of variables manually via $a"
                                   , constructorFields    = [ (defaultField :: Field (Application Concrete Concrete))
                                                              { fieldName      = "variable"
                                                              , fieldType      = Variable First
                                                              }
                                                            ]
                                   } 
                                 , (defaultConstructor :: Constructor  (Application Concrete Concrete) )
                                   { constructorName      = "Application"
                                   , constructorSemantics = "Apply one type onto another yielding a type of lower kind."
                                   , constructorFields    = [ (defaultField :: Field (Application Concrete Concrete))
                                                              { fieldName      = "function"
                                                              , fieldType      = Application 
                                                                                   (DataType "0174bd22-6400-4820-bfe3-4e211cb35a7d")
                                                                                   (Variable First)
                                                              }
                                                            , (defaultField :: Field (Application Concrete Concrete))
                                                              { fieldName      = "argument"
                                                              , fieldType      = Application
                                                                                   (DataType "0174bd22-6400-4820-bfe3-4e211cb35a7d")
                                                                                   (Variable First)
                                                              }
                                                            ]
                                   } 
                                 , (defaultConstructor :: Constructor  (Application Concrete Concrete) )
                                   { constructorName      = "Quantification"
                                   , constructorSemantics = "Existential quantification."
                                   , constructorFields    = [ (defaultField :: Field (Application Concrete Concrete))
                                                              { fieldName      = "constraints"
                                                              , fieldType      = Application 
                                                                                   (DataType "7af30cce93724981a16a80f3f193dc33")
                                                                                   (Application 
                                                                                     (DataType "2a94a7a8d4e049759d8dd546e72293ff")
                                                                                     (Application
                                                                                       (DataType "9e2e1e478e094a8abe5507f8574ac91f")
                                                                                       (Variable First)
                                                                                     )
                                                                                   )
                                                              }
                                                            , (defaultField :: Field (Application Concrete Concrete))
                                                              { fieldName      = "forall"
                                                              , fieldType      = Application
                                                                                   (DataType "0174bd22-6400-4820-bfe3-4e211cb35a7d")
                                                                                   (Application
                                                                                     (DataType "9e2e1e478e094a8abe5507f8574ac91f")
                                                                                     (Variable First)
                                                                                   )
                                                              }
                                                            ]
                                   } 
                                ]
                       }
                       
t36     :: TypeDefinition Concrete
t36      = defaultType {
             identifier   = "4f7db06c-4395-4165-8a09-689d3e7dd909"
           , name         = "Text"
           , semantics    = "Unicode text. This is the default type for text processing (NLP etc)."
           , constructors = Nothing
          }

t37     :: TypeDefinition Concrete
t37      = defaultType {
             identifier   = "f9f2f27a-f0f6-49b4-bc89-46c467c3b76a"
           , name         = "ByteString"
           , semantics    = "Sequence of Bytes (8bit). This is the default type for storing binary data. This type is not to be used for storing text! Use -> Text instead."
           , constructors = Nothing
          }
                       
t38     :: TypeDefinition Concrete
t38      = defaultType {
             identifier   = "c211e54d-6eef-4234-a7b6-75d5f696efe5"
           , name         = "Rational"
           , semantics    = "A rational number. May be implemented as a fraction of -> Integer and -> Wordeger."
           , constructors = Nothing
          }

t39     :: TypeDefinition Concrete
t39      = defaultType {
             identifier   = "5e5c664c-fc32-4271-b542-bf7ab0c9c104"
           , name         = "RationalUnsigned"
           , semantics    = "An unsigned rational number. May be implemented as a fraction of two -> Wordeger."
           , constructors = Nothing
          }

t3      :: TypeDefinition Concrete
t3       = defaultType {
             identifier   = "16f4245df3cc0b534f028235ff8aae16"
           , name         = "Char"
           , semantics    = "A point in the Unicode space."
           , constructors = Nothing
          }

          
t4      :: TypeDefinition Concrete
t4       = defaultType {
             identifier   = "c74c35ddb3ef689646c50be868d11bdf"
           , name         = "Float"
           , semantics    = "Single precision floating point number (IEEE 754)"
           , constructors = Nothing
          }

                    
t5      :: TypeDefinition Concrete
t5       = defaultType {
             identifier   = "4b19d19d959322ac0ccd319a4d275bd0"
           , name         = "Double"
           , semantics    = "Double precision floating point number (IEEE 754)"
           , constructors = Nothing
          }


t6      :: TypeDefinition Concrete
t6       = defaultType {
             identifier   = "ec78dc6268e4fe6fe6df461f40359d62"
           , name         = "Int8"
           , semantics    = "Signed integer types with well defined range. These types are to be used if it is important to emphasize the size limitations."
           , constructors = Nothing
          }


t7      :: TypeDefinition Concrete
t7       = defaultType {
             identifier   = "7ee200d207963cca2d2a49719e97e973"
           , name         = "Int16"
           , semantics    = "Signed integer types with well defined range. These types are to be used if it is important to emphasize the size limitations."
           , constructors = Nothing
          }

t8      :: TypeDefinition Concrete
t8       = defaultType {
             identifier   = "7b05ee3f0bbe6569f48d3947ec425493"
           , name         = "Int32"
           , semantics    = "Signed integer types with well defined range. These types are to be used if it is important to emphasize the size limitations."
           , constructors = Nothing
          }

t9      :: TypeDefinition Concrete
t9       = defaultType {
             identifier   = "cc620c86261c781e03c8efd9a974b1cf"
           , name         = "Int64"
           , semantics    = "Signed integer types with well defined range. These types are to be used if it is important to emphasize the size limitations."
           , constructors = Nothing
          }

t10      :: TypeDefinition Concrete
t10       = defaultType {
             identifier   = "7704e26b08886d6b8c3c788a3a0b2db0)"
           , name         = "Word8"
           , semantics    = "unsigned integer types with well defined range. These types are to be used if it is important to emphasize the size limitations."
           , constructors = Nothing
          }

t11      :: TypeDefinition Concrete
t11      = defaultType {
             identifier   = "2b567f4ccc26027e, 0xa78edd227800fe94"
           , name         = "Word16"
           , semantics    = "unsigned integer types with well defined range. These types are to be used if it is important to emphasize the size limitations."
           , constructors = Nothing
          }

t12      :: TypeDefinition Concrete
t12       = defaultType {
             identifier   = "1a55145e5bd21e8adc14067707192552"
           , name         = "Word32"
           , semantics    = "unsigned integer types with well defined range. These types are to be used if it is important to emphasize the size limitations."
           , constructors = Nothing
          }

t13      :: TypeDefinition Concrete
t13       = defaultType {
             identifier   = "187e33b43715d8fe529de5014c864d85"
           , name         = "Word64"
           , semantics    = "unsigned integer types with well defined range. These types are to be used if it is important to emphasize the size limitations."
           , constructors = Nothing
          }

t14      :: TypeDefinition Concrete
t14       = defaultType {
             identifier   = "bbabbac1-510d-49aa-9da2-5d8033147c54"
           , name         = "Word128"
           , semantics    = "unsigned integer types with well defined range. These types are to be used if it is important to emphasize the size limitations."
           , constructors = Nothing
          }

t15      :: TypeDefinition Concrete
t15       = defaultType {
             identifier   = "90ce401b-d12d-4afc-be37-331bed9e0423"
           , name         = "Word256"
           , semantics    = "unsigned integer types with well defined range. These types are to be used if it is important to emphasize the size limitations."
           , constructors = Nothing
          }

t16      :: TypeDefinition Concrete
t16       = defaultType {
             identifier   = "c15eddf1-94ad-4428-8cba-be701d5ae517"
           , name         = "Word512"
           , semantics    = "unsigned integer types with well defined range. These types are to be used if it is important to emphasize the size limitations."
           , constructors = Nothing
          }

t17      :: TypeDefinition Concrete
t17       = defaultType {
             identifier   = "ac2e770f2132aced749ec197385ff552"
           , name         = "Int"
           , semantics    = "Signed integer. This type is to be used as the default signed integer type. Due to technical constraints the type is bounded. It is assumed that in most applications this bound doesn't get exceeded. If this happens to a problem use the arbitrary precision integer types."
           , constructors = Nothing
          }



t18      :: TypeDefinition Concrete
t18       = defaultType {
             identifier   = "8006b4b18388f841272dbebeee847723"
           , name         = "Integer"
           , semantics    = "Signed integer with arbitrary precision only limited by the machined resources. (Macht Integer bis der Speicher platzt!)."
           , constructors = Nothing
          }



t19      :: TypeDefinition Concrete
t19       = defaultType {
             identifier   = "62d2d537-1f08-461a-a328-bc06561594f6"
           , name         = "Word"
           , semantics    = "unsigned integer types with well defined range. These types are to be used if it is important to emphasize the size limitations."
           , constructors = Nothing
          }



t20     :: TypeDefinition Concrete
t20       = defaultType {
             identifier   = "982dce09-43f6-4a74-858f-f22c753ab01d"
           , name         = "Wordeger"
           , semantics    = "unsigned integer types with arbitrary precision only limited by the machined resources."
           , constructors = Nothing
          }


t21     :: TypeDefinition (Application Concrete (Application Concrete Concrete)) 
t21       = defaultType {
             identifier   = "d9eef038b47d0820c160ceb8b6a89943"
           , name         = "Either"
           , semantics    = "Denotes a choice between two possible values of different types. The Left value is typically used for something that informs about the reason of a failed computation"
           , constructors = Just [
                                   (defaultConstructor  :: Constructor  (Application Concrete (Application Concrete Concrete)) )
                                     { constructorName      = "Left"
                                     , constructorSemantics = "This constructor denotes the unwanted result"
                                     , constructorFields    = [ (defaultField :: Field (Application Concrete (Application Concrete Concrete)) )
                                                                  { fieldName      = "left"
                                                                  , fieldSemantics = ""
                                                                  , fieldType      = (Variable First)
                                                                  }
                                                              ]
                                     } 
                                 , (defaultConstructor :: Constructor  (Application Concrete (Application Concrete Concrete)) )
                                     { constructorName      = "Right"
                                     , constructorSemantics = "This constructor denotes the wanted result"
                                     , constructorFields    = [ (defaultField :: Field (Application Concrete (Application Concrete Concrete)) )
                                                                  { fieldName      = "left"
                                                                  , fieldSemantics = ""
                                                                  , fieldType      = (Variable (Next First))
                                                                  }
                                                              ]
                                     } 
                                 ]

          }


t22     :: TypeDefinition (Application Concrete Concrete) 
t22      = defaultType {
             identifier   = "f8f49ef6bbe874a42926fa23d5b3bc19"
           , name         = "Maybe"
           , semantics    = "Expresses optionality."
           , constructors = Just [
                                   (defaultConstructor :: Constructor (Application Concrete Concrete))
                                     { constructorName      = "Nothing"
                                     , constructorSemantics = "This constructor denotes the absence of a value"
                                     } 
                                 ,  (defaultConstructor :: Constructor (Application Concrete Concrete))
                                     { constructorName      = "Just"
                                     , constructorSemantics = "This constructor wraps a value "
                                     , constructorFields    = [ defaultField
                                                                  { fieldName      = "just"
                                                                  , fieldType      = (Variable First)
                                                                  }
                                                              ]
                                     } 
                                 ]

          }


t23     :: TypeDefinition Concrete 
t23       = defaultType {
             identifier   = "10f280df659654becb6e08122e846284"
           , name         = "Unit"
           , semantics    = "The empty tuple. () in Haskell syntax."
           , constructors = Just [defaultConstructor {constructorName = "Unit"}]
          }

t24     :: TypeDefinition (Application Concrete (Application Concrete Concrete)) 
t24       = defaultType {
             identifier   = "34c13bdaac7d413ed735e64edcac7ff5"
           , name         = "Tuple"
           , semantics    = "A tuple."
           , constructors = Just [
                                  defaultConstructor 
                                    { constructorName = "Tuple"
                                    , constructorFields = [ defaultField
                                                              { fieldName = "fst"
                                                              , fieldType = (Variable First)
                                                              }
                                                              
                                                            , defaultField
                                                              { fieldName = "snd"
                                                              ,  fieldType = (Variable (Next First))
                                                              }
                                                          ]
                                    
                                    }
                                  
                                 ]
          }

t91     :: TypeDefinition Concrete 
t91       = defaultType
            { identifier    = "606d6a25-5371-4be6-b046-7d2eb2c0708e"
            , name          = "Concrete"
            , author        = Just personLars
            , semantics     = "The concrete kind."
            , constructors  = Just []
            }

t92     :: TypeDefinition (Application Concrete (Application Concrete Concrete))
t92       = defaultType
            { identifier    = "be46fbdd-7467-43fe-9ef3-9717c30b21fb"
            , name          = "Application"
            , author        = Just personLars
            , semantics     = "The concrete kind."
            , constraints   = S.fromList [Constraint "c6ebaa9f4cdc4068894d1ffaef5a7a83" [Variable First], Constraint "c6ebaa9f4cdc4068894d1ffaef5a7a83" [Variable (Next First)]]
            , constructors  = Just [ 
                                     defaultConstructor
                                     { constructorName      = "First"
                                     , constructorSemantics = "The first variable."
                                     }
                                   , (defaultConstructor :: Constructor  (Application Concrete (Application Concrete Concrete)))
                                     { constructorName      = "Next"
                                     , constructorSemantics = "The next variable. Only applicable if $b is of higher kind."
                                     , constructorFields    = [ (defaultField :: Field (Application Concrete (Application Concrete Concrete)))
                                                                { fieldName      = "previous"
                                                                , fieldSemantics = "yields the kind of the codomain"
                                                                , fieldType      = Variable (Next First)
                                                                }
                                                              ]
                                     }
                                   ]
            }


t90     :: TypeDefinition (Application Concrete (Application Concrete (Application Concrete Concrete)))
t90       = defaultType {
             identifier   = "5cae969a-6574-48eb-bc2e-af7f31d7340f"
           , name         = "Triple"
           , semantics    = "A triple."
           , constructors = Just [
                                  defaultConstructor 
                                    { constructorName = "Triple"
                                    , constructorFields = [ defaultField
                                                              { fieldName = "fst"
                                                              , fieldType = (Variable First)
                                                              }
                                                            , defaultField
                                                              { fieldName = "snd"
                                                              ,  fieldType = (Variable (Next First))
                                                              }
                                                            , defaultField
                                                              { fieldName = "thrd"
                                                              ,  fieldType = (Variable (Next (Next First)))
                                                              }
                                                          ]
                                    
                                    }
                                  
                                 ]
          }



t25     :: TypeDefinition Concrete 
t25       = defaultType {
             identifier   = "ff421b2c-3177-4c37-a733-6c8245a74da9"
           , author       = Just personClemens
           , name         = "DecimalAlphabet"
           , semantics    = "The 10 ciphers of the decimal system"
           , constructors = Just [
                                   defaultConstructor { constructorName = "Zero"  }
                                 , defaultConstructor { constructorName = "One"   }
                                 , defaultConstructor { constructorName = "Two"   }
                                 , defaultConstructor { constructorName = "Three" }
                                 , defaultConstructor { constructorName = "Four"  }
                                 , defaultConstructor { constructorName = "Five"  }
                                 , defaultConstructor { constructorName = "Six"   }
                                 , defaultConstructor { constructorName = "Seven" }
                                 , defaultConstructor { constructorName = "Eight" }
                                 , defaultConstructor { constructorName = "Nine"  }
                                 
                                 ]
          }


t26     :: TypeDefinition Concrete 
t26       = defaultType {
             identifier   = "45cc309e-ec2d-47f3-a7ed-3af50c84a392"
           , author       = Just personClemens
           , name         = "HexadecimalAlphabet"
           , semantics    = "The 10 ciphers of the hexadecimal system"
           , constructors = Just [
                                   defaultConstructor { constructorName = "Zero"  }
                                 , defaultConstructor { constructorName = "One"   }
                                 , defaultConstructor { constructorName = "Two"   }
                                 , defaultConstructor { constructorName = "Three" }
                                 , defaultConstructor { constructorName = "Four"  }
                                 , defaultConstructor { constructorName = "Five"  }
                                 , defaultConstructor { constructorName = "Six"   }
                                 , defaultConstructor { constructorName = "Seven" }
                                 , defaultConstructor { constructorName = "Eight" }
                                 , defaultConstructor { constructorName = "Nine"  }
                                 , defaultConstructor { constructorName = "Ten"  }
                                 , defaultConstructor { constructorName = "Eleven"  }
                                 , defaultConstructor { constructorName = "Twelve"  }
                                 , defaultConstructor { constructorName = "Thirteen"  }
                                 , defaultConstructor { constructorName = "Fourteen"  }
                                 , defaultConstructor { constructorName = "Fifteen"  }                                                                  
                                 
                                 ]
          }
          
 
t27     :: TypeDefinition Concrete 
t27       = defaultType {
             identifier   = "6716d098-a587-4337-9e54-c12f249cdc0c"
           , author       = Just personClemens
           , name         = "LatinAlphabet"
           , semantics    = "The 26 letters of the latin alphabet"
           , constructors = Just [
                                   defaultConstructor { constructorName = "A"  }
                                 , defaultConstructor { constructorName = "B"   }
                                 , defaultConstructor { constructorName = "C"   }
                                 , defaultConstructor { constructorName = "D" }
                                 , defaultConstructor { constructorName = "E"  }
                                 , defaultConstructor { constructorName = "F"  }
                                 , defaultConstructor { constructorName = "G"   }
                                 , defaultConstructor { constructorName = "H" }
                                 , defaultConstructor { constructorName = "I" }
                                 , defaultConstructor { constructorName = "J"  }
                                 , defaultConstructor { constructorName = "K"  }
                                 , defaultConstructor { constructorName = "L"  }
                                 , defaultConstructor { constructorName = "M"  }
                                 , defaultConstructor { constructorName = "N"  }
                                 , defaultConstructor { constructorName = "O"  }
                                 , defaultConstructor { constructorName = "P"  }  
                                 , defaultConstructor { constructorName = "Q"  }
                                 , defaultConstructor { constructorName = "R"  }
                                 , defaultConstructor { constructorName = "S"  }
                                 , defaultConstructor { constructorName = "T"  }
                                 , defaultConstructor { constructorName = "U"  }
                                 , defaultConstructor { constructorName = "V"  }
                                 , defaultConstructor { constructorName = "W"  }
                                 , defaultConstructor { constructorName = "X"  }
                                 , defaultConstructor { constructorName = "Y"  }
                                 , defaultConstructor { constructorName = "Z"  }                                                                
                                 
                                 ]
          }          
          

t28     :: TypeDefinition Concrete 
t28       = defaultType {
             identifier   = "1566edb1-a4de-4aab-8106-e63293e9bfcf"
           , author       = Just personClemens
           , name         = "Symbol"
           , semantics    = "the choice of symbols available in a -> Designator: Lower and Uppercase Latin Characters, decimal ciphers and the underscore"
           , constructors = Just [
                                   defaultConstructor { 
                                     constructorName = "Lower"
                                    ,constructorFields = [ defaultField {
                                                                   fieldName = "lower" 
                                                                 , fieldType = DataType "6716d098-a587-4337-9e54-c12f249cdc0c"  
                                                                 }
                                                         ]
                                   }
                                 , defaultConstructor { 
                                     constructorName = "Upper"
                                    ,constructorFields = [ defaultField {
                                                                   fieldName = "upper" 
                                                                 , fieldType = DataType "6716d098-a587-4337-9e54-c12f249cdc0c"  
                                                                 }
                                                         ]
                                   } 
                                   
                                 , defaultConstructor { 
                                     constructorName = "Decimal"
                                    ,constructorFields =  [ defaultField {
                                                                   fieldName = "decimal" 
                                                                ,  fieldType = DataType "ff421b2c-3177-4c37-a733-6c8245a74da9"
                                                                 }
                                                          ]
                                   }  
                                   
                                 , defaultConstructor { 
                                     constructorName = "Underscore"
                                    ,constructorFields = []
                                   }    
                                 , defaultConstructor { 
                                     constructorName = "Prime"
                                    ,constructorFields = []
                                   }
                                 ]
          }
          
t29     :: TypeDefinition Concrete 
t29       = defaultType {
             identifier   = "9790ade9-814a-4aac-a5ea-a80c3e47685d"
           , author       = Just personClemens
           , name         = "LowerDesignator"
           , semantics    = "This type represents valid designators. These contain at least one -> latin character that is lowercase, and a list of -> Symbols (lower/upper character, decimal character, underscore) of arbitrary length. Also look how the type itself determines the semantics of the single LatinAlphabet character: It itself does not have any case but the context induces one."
           , constructors = Just [
                                   defaultConstructor { 
                                     constructorName = "Designator"
                                    ,constructorFields = [ defaultField {
                                                                   fieldName = "initial" 
                                                                ,  fieldType = DataType "6716d098-a587-4337-9e54-c12f249cdc0c"  
                                                                 }
                                                         , defaultField {
                                                                   fieldName = "rest" 
                                                                ,  fieldType = list $ DataType "1566edb1-a4de-4aab-8106-e63293e9bfcf"
                                                                 }
                                                         ]
                                   }           
                                 ]
          }   
          
          
          
          
t30     :: TypeDefinition Concrete 
t30       = defaultType {
             identifier   = "44d86fd3-a506-477a-b886-83d796e0d18b"
           , author       = Just personClemens
           , name         = "UpperDesignator"
           , semantics    = "This type represents valid designators. These contain at least one -> latin character that is uppercase, a list of -> Symbol (lower/upper character, decimal character, underscore) of arbitrary length. Also look how the type itself determines the semantics of the single LatinAlphabet character: It itself does not have any case but the context induces one."
           , constructors = Just [
                                   defaultConstructor { 
                                     constructorName = "Designator"
                                    ,constructorFields = [ defaultField {
                                                                   fieldName = "initial" 
                                                                ,  fieldType = DataType "6716d098-a587-4337-9e54-c12f249cdc0c"  
                                                                 }
                                                         , defaultField {
                                                                   fieldName = "rest" 
                                                                ,  fieldType = list $ DataType "1566edb1-a4de-4aab-8106-e63293e9bfcf"
                                                                 }
                                                         ]
                                   }           
                                 ]
          }  
          

t31     :: TypeDefinition (Application Concrete Concrete) 
t31       = defaultType {
             identifier   = "7af30cce-9372-4981-a16a-80f3f193dc33"
           , name         = "Set"
           , semantics    = "A set. Elements are unique and unordered"
           , constructors = Nothing
          }         
          
          

t32     :: TypeDefinition (Application Concrete (Application Concrete Concrete)) 
t32       = defaultType {
             identifier   = "43c6cd13-33b0-4fc8-a480-668ecb24768e"
           , name         = "Map"
           , semantics    = "Associates keys of type a with elements of type b."
           , constructors = Nothing
          }               

t33     :: TypeDefinition Concrete 
t33       = defaultType {
             identifier   = "34667404-2a72-48b4-a94a-bff0726d0c43"
           , name         = "UUID"
           , semantics    = "A universally unique identifier. It is defined to be a natural number in range 0 to 2^128-1. For all practical applications this type is assumed to provide infinitely many values. The order induced by the definition is to be ignored."
           , constructors = Just [
                                  defaultConstructor 
                                    { constructorName = "UUID"
                                    , constructorFields = [ defaultField             
                                                              { fieldName = "uuid"
                                                              , fieldType = DataType  "bbabbac1510d49aa9da25d8033147c54"
                                                              }
                                                          ]
                                    
                                    }
                                 ]
          }

t40     :: TypeDefinition (Application Concrete Concrete)
t40       = defaultType {
             identifier   = "606f2535-33d3-420d-a346-5afae341d598"
           , author       = Just personMikael
           , name         = "Time"
           , semantics    = "This type is used for noting a point in time. It is polymorphic in the timescale used. See http://en.wikipedia.org/wiki/Time_standard for details on this issue."
           , constraints  = S.fromList [Constraint "882f4a6a-ffa2-4579-830e-0a850acad145" [Variable First]]
           , constructors = Just [
                                  defaultConstructor 
                                    { constructorName = "Time"
                                    , constructorFields = [ (defaultField :: Field  (Application Concrete Concrete))
                                                              { fieldName      = "seconds"
                                                              , fieldType      = DataType  "c211e54d6eef4234a7b675d5f696efe5"
                                                              , fieldSemantics = "Seconds relative to January 1st, 1900. What is meant by a second depends on the timescale."
                                                              }
                                                          ]
                                    
                                    }
                                 ]
          }


t50     :: TypeDefinition (Application Concrete Concrete) 
t50       = defaultType {
             identifier   = "b6831ec097f14b8eba74b1e486b4175d"
           , author       = Just personLars
           , name         = "Date"
           , semantics    = "This type is used for noting a day time. It is polymorphic in the timescale used. See http://en.wikipedia.org/wiki/Time_standard for details on this issue."
           , constraints  = S.fromList [Constraint "882f4a6a-ffa2-4579-830e-0a850acad145" [Variable First]]
           , constructors = Just [
                                  defaultConstructor 
                                    { constructorName = "Time"
                                    , constructorFields = [ (defaultField  :: Field  (Application Concrete Concrete))           
                                                              { fieldName      = "days"
                                                              , fieldType      = DataType "ac2e770f2132aced749ec197385ff552"
                                                              , fieldSemantics = "Days relative to January 1st, 1900. What is meant by a day depends on the timescale."
                                                              }
                                                          ]
                                    
                                    }
                                 ]
          }



t34     :: TypeDefinition Concrete 
t34       = defaultType {
             identifier   = "f47867c1-1a4d-4e30-ab65-2240dd8e72ba"
           , name         = "Void"
           , semantics    = "This type has no constructors and therefore no instances. Don't mix it up with -> Nil which has exactly one instance."
           , constructors = Just []
          }               

t35     :: TypeDefinition Concrete 
t35       = defaultType {
             identifier   = "af20e1db-8f0d-414f-9062-5b1521e41378"
           , name         = "Language"
           , semantics    = "Languages according to ISO 639-2T."
           , constructors = Just [
                                    defaultConstructor { constructorName = "AAR", constructorSemantics = "Afar" }
                                  , defaultConstructor { constructorName = "ABK", constructorSemantics = "Abkhazian" }
                                  , defaultConstructor { constructorName = "ACE", constructorSemantics = "Achinese" }
                                  , defaultConstructor { constructorName = "ACH", constructorSemantics = "Acoli" }
                                  , defaultConstructor { constructorName = "ADA", constructorSemantics = "Adangme" }
                                  , defaultConstructor { constructorName = "ADY", constructorSemantics = "Adyghe; Adygei" }
                                  , defaultConstructor { constructorName = "AFA", constructorSemantics = "Afro-Asiatic languages" }
                                  , defaultConstructor { constructorName = "AFH", constructorSemantics = "Afrihili" }
                                  , defaultConstructor { constructorName = "AFR", constructorSemantics = "Afrikaans" }
                                  , defaultConstructor { constructorName = "AIN", constructorSemantics = "Ainu" }
                                  , defaultConstructor { constructorName = "AKA", constructorSemantics = "Akan" }
                                  , defaultConstructor { constructorName = "AKK", constructorSemantics = "Akkadian" }
                                  , defaultConstructor { constructorName = "ALE", constructorSemantics = "Aleut" }
                                  , defaultConstructor { constructorName = "ALG", constructorSemantics = "Algonquian languages" }
                                  , defaultConstructor { constructorName = "ALT", constructorSemantics = "Southern Altai" }
                                  , defaultConstructor { constructorName = "AMH", constructorSemantics = "Amharic" }
                                  , defaultConstructor { constructorName = "ANG", constructorSemantics = "English, Old (ca.450-1100)" }
                                  , defaultConstructor { constructorName = "ANP", constructorSemantics = "Angika" }
                                  , defaultConstructor { constructorName = "APA", constructorSemantics = "Apache languages" }
                                  , defaultConstructor { constructorName = "ARA", constructorSemantics = "Arabic" }
                                  , defaultConstructor { constructorName = "ARC", constructorSemantics = "Official Aramaic (700-300 BCE); Imperial Aramaic (700-300 BCE)" }
                                  , defaultConstructor { constructorName = "ARG", constructorSemantics = "Aragonese" }
                                  , defaultConstructor { constructorName = "ARN", constructorSemantics = "Mapudungun; Mapuche" }
                                  , defaultConstructor { constructorName = "ARP", constructorSemantics = "Arapaho" }
                                  , defaultConstructor { constructorName = "ART", constructorSemantics = "Artificial languages" }
                                  , defaultConstructor { constructorName = "ARW", constructorSemantics = "Arawak" }
                                  , defaultConstructor { constructorName = "ASM", constructorSemantics = "Assamese" }
                                  , defaultConstructor { constructorName = "AST", constructorSemantics = "Asturian; Bable; Leonese; Asturleonese" }
                                  , defaultConstructor { constructorName = "ATH", constructorSemantics = "Athapascan languages" }
                                  , defaultConstructor { constructorName = "AUS", constructorSemantics = "Australian languages" }
                                  , defaultConstructor { constructorName = "AVA", constructorSemantics = "Avaric" }
                                  , defaultConstructor { constructorName = "AVE", constructorSemantics = "Avestan" }
                                  , defaultConstructor { constructorName = "AWA", constructorSemantics = "Awadhi" }
                                  , defaultConstructor { constructorName = "AYM", constructorSemantics = "Aymara" }
                                  , defaultConstructor { constructorName = "AZE", constructorSemantics = "Azerbaijani" }
                                  , defaultConstructor { constructorName = "BAD", constructorSemantics = "Banda languages" }
                                  , defaultConstructor { constructorName = "BAI", constructorSemantics = "Bamileke languages" }
                                  , defaultConstructor { constructorName = "BAK", constructorSemantics = "Bashkir" }
                                  , defaultConstructor { constructorName = "BAL", constructorSemantics = "Baluchi" }
                                  , defaultConstructor { constructorName = "BAM", constructorSemantics = "Bambara" }
                                  , defaultConstructor { constructorName = "BAN", constructorSemantics = "Balinese" }
                                  , defaultConstructor { constructorName = "BAS", constructorSemantics = "Basa" }
                                  , defaultConstructor { constructorName = "BAT", constructorSemantics = "Baltic languages" }
                                  , defaultConstructor { constructorName = "BEJ", constructorSemantics = "Beja; Bedawiyet" }
                                  , defaultConstructor { constructorName = "BEL", constructorSemantics = "Belarusian" }
                                  , defaultConstructor { constructorName = "BEM", constructorSemantics = "Bemba" }
                                  , defaultConstructor { constructorName = "BEN", constructorSemantics = "Bengali" }
                                  , defaultConstructor { constructorName = "BER", constructorSemantics = "Berber languages" }
                                  , defaultConstructor { constructorName = "BHO", constructorSemantics = "Bhojpuri" }
                                  , defaultConstructor { constructorName = "BIH", constructorSemantics = "Bihari languages" }
                                  , defaultConstructor { constructorName = "BIK", constructorSemantics = "Bikol" }
                                  , defaultConstructor { constructorName = "BIN", constructorSemantics = "Bini; Edo" }
                                  , defaultConstructor { constructorName = "BIS", constructorSemantics = "Bislama" }
                                  , defaultConstructor { constructorName = "BLA", constructorSemantics = "Siksika" }
                                  , defaultConstructor { constructorName = "BNT", constructorSemantics = "Bantu languages" }
                                  , defaultConstructor { constructorName = "BOD", constructorSemantics = "Tibetan" }
                                  , defaultConstructor { constructorName = "BOS", constructorSemantics = "Bosnian" }
                                  , defaultConstructor { constructorName = "BRA", constructorSemantics = "Braj" }
                                  , defaultConstructor { constructorName = "BRE", constructorSemantics = "Breton" }
                                  , defaultConstructor { constructorName = "BTK", constructorSemantics = "Batak languages" }
                                  , defaultConstructor { constructorName = "BUA", constructorSemantics = "Buriat" }
                                  , defaultConstructor { constructorName = "BUG", constructorSemantics = "Buginese" }
                                  , defaultConstructor { constructorName = "BUL", constructorSemantics = "Bulgarian" }
                                  , defaultConstructor { constructorName = "BYN", constructorSemantics = "Blin; Bilin" }
                                  , defaultConstructor { constructorName = "CAD", constructorSemantics = "Caddo" }
                                  , defaultConstructor { constructorName = "CAI", constructorSemantics = "Central American Indian languages" }
                                  , defaultConstructor { constructorName = "CAR", constructorSemantics = "Galibi Carib" }
                                  , defaultConstructor { constructorName = "CAT", constructorSemantics = "Catalan; Valencian" }
                                  , defaultConstructor { constructorName = "CAU", constructorSemantics = "Caucasian languages" }
                                  , defaultConstructor { constructorName = "CEB", constructorSemantics = "Cebuano" }
                                  , defaultConstructor { constructorName = "CEL", constructorSemantics = "Celtic languages" }
                                  , defaultConstructor { constructorName = "CES", constructorSemantics = "Czech" }
                                  , defaultConstructor { constructorName = "CHA", constructorSemantics = "Chamorro" }
                                  , defaultConstructor { constructorName = "CHB", constructorSemantics = "Chibcha" }
                                  , defaultConstructor { constructorName = "CHE", constructorSemantics = "Chechen" }
                                  , defaultConstructor { constructorName = "CHG", constructorSemantics = "Chagatai" }
                                  , defaultConstructor { constructorName = "CHK", constructorSemantics = "Chuukese" }
                                  , defaultConstructor { constructorName = "CHM", constructorSemantics = "Mari" }
                                  , defaultConstructor { constructorName = "CHN", constructorSemantics = "Chinook jargon" }
                                  , defaultConstructor { constructorName = "CHO", constructorSemantics = "Choctaw" }
                                  , defaultConstructor { constructorName = "CHP", constructorSemantics = "Chipewyan; Dene Suline" }
                                  , defaultConstructor { constructorName = "CHR", constructorSemantics = "Cherokee" }
                                  , defaultConstructor { constructorName = "CHU", constructorSemantics = "Church Slavic; Old Slavonic; Church Slavonic; Old Bulgarian; Old Church Slavonic" }
                                  , defaultConstructor { constructorName = "CHV", constructorSemantics = "Chuvash" }
                                  , defaultConstructor { constructorName = "CHY", constructorSemantics = "Cheyenne" }
                                  , defaultConstructor { constructorName = "CMC", constructorSemantics = "Chamic languages" }
                                  , defaultConstructor { constructorName = "COP", constructorSemantics = "Coptic" }
                                  , defaultConstructor { constructorName = "COR", constructorSemantics = "Cornish" }
                                  , defaultConstructor { constructorName = "COS", constructorSemantics = "Corsican" }
                                  , defaultConstructor { constructorName = "CPE", constructorSemantics = "Creoles and pidgins, English based" }
                                  , defaultConstructor { constructorName = "CPF", constructorSemantics = "Creoles and pidgins, French-based" }
                                  , defaultConstructor { constructorName = "CPP", constructorSemantics = "Creoles and pidgins, Portuguese-based" }
                                  , defaultConstructor { constructorName = "CRE", constructorSemantics = "Cree" }
                                  , defaultConstructor { constructorName = "CRH", constructorSemantics = "Crimean Tatar; Crimean Turkish" }
                                  , defaultConstructor { constructorName = "CRP", constructorSemantics = "Creoles and pidgins" }
                                  , defaultConstructor { constructorName = "CSB", constructorSemantics = "Kashubian" }
                                  , defaultConstructor { constructorName = "CUS", constructorSemantics = "Cushitic languages" }
                                  , defaultConstructor { constructorName = "CYM", constructorSemantics = "Welsh" }
                                  , defaultConstructor { constructorName = "DAK", constructorSemantics = "Dakota" }
                                  , defaultConstructor { constructorName = "DAN", constructorSemantics = "Danish" }
                                  , defaultConstructor { constructorName = "DAR", constructorSemantics = "Dargwa" }
                                  , defaultConstructor { constructorName = "DAY", constructorSemantics = "Land Dayak languages" }
                                  , defaultConstructor { constructorName = "DEL", constructorSemantics = "Delaware" }
                                  , defaultConstructor { constructorName = "DEN", constructorSemantics = "Slave (Athapascan)" }
                                  , defaultConstructor { constructorName = "DEU", constructorSemantics = "German" }
                                  , defaultConstructor { constructorName = "DGR", constructorSemantics = "Dogrib" }
                                  , defaultConstructor { constructorName = "DIN", constructorSemantics = "Dinka" }
                                  , defaultConstructor { constructorName = "DIV", constructorSemantics = "Divehi; Dhivehi; Maldivian" }
                                  , defaultConstructor { constructorName = "DOI", constructorSemantics = "Dogri" }
                                  , defaultConstructor { constructorName = "DRA", constructorSemantics = "Dravidian languages" }
                                  , defaultConstructor { constructorName = "DSB", constructorSemantics = "Lower Sorbian" }
                                  , defaultConstructor { constructorName = "DUA", constructorSemantics = "Duala" }
                                  , defaultConstructor { constructorName = "DUM", constructorSemantics = "Dutch, Middle (ca.1050-1350)" }
                                  , defaultConstructor { constructorName = "DYU", constructorSemantics = "Dyula" }
                                  , defaultConstructor { constructorName = "DZO", constructorSemantics = "Dzongkha" }
                                  , defaultConstructor { constructorName = "EFI", constructorSemantics = "Efik" }
                                  , defaultConstructor { constructorName = "EGY", constructorSemantics = "Egyptian (Ancient)" }
                                  , defaultConstructor { constructorName = "EKA", constructorSemantics = "Ekajuk" }
                                  , defaultConstructor { constructorName = "ELL", constructorSemantics = "Greek, Modern (1453-)" }
                                  , defaultConstructor { constructorName = "ELX", constructorSemantics = "Elamite" }
                                  , defaultConstructor { constructorName = "ENG", constructorSemantics = "English" }
                                  , defaultConstructor { constructorName = "ENM", constructorSemantics = "English, Middle (1100-1500)" }
                                  , defaultConstructor { constructorName = "EPO", constructorSemantics = "Esperanto" }
                                  , defaultConstructor { constructorName = "EST", constructorSemantics = "Estonian" }
                                  , defaultConstructor { constructorName = "EUS", constructorSemantics = "Basque" }
                                  , defaultConstructor { constructorName = "EWE", constructorSemantics = "Ewe" }
                                  , defaultConstructor { constructorName = "EWO", constructorSemantics = "Ewondo" }
                                  , defaultConstructor { constructorName = "FAN", constructorSemantics = "Fang" }
                                  , defaultConstructor { constructorName = "FAO", constructorSemantics = "Faroese" }
                                  , defaultConstructor { constructorName = "FAS", constructorSemantics = "Persian" }
                                  , defaultConstructor { constructorName = "FAT", constructorSemantics = "Fanti" }
                                  , defaultConstructor { constructorName = "FIJ", constructorSemantics = "Fijian" }
                                  , defaultConstructor { constructorName = "FIL", constructorSemantics = "Filipino; Pilipino" }
                                  , defaultConstructor { constructorName = "FIN", constructorSemantics = "Finnish" }
                                  , defaultConstructor { constructorName = "FIU", constructorSemantics = "Finno-Ugrian languages" }
                                  , defaultConstructor { constructorName = "FON", constructorSemantics = "Fon" }
                                  , defaultConstructor { constructorName = "FRA", constructorSemantics = "French" }
                                  , defaultConstructor { constructorName = "FRM", constructorSemantics = "French, Middle (ca.1400-1600)" }
                                  , defaultConstructor { constructorName = "FRO", constructorSemantics = "French, Old (842-ca.1400)" }
                                  , defaultConstructor { constructorName = "FRR", constructorSemantics = "Northern Frisian" }
                                  , defaultConstructor { constructorName = "FRS", constructorSemantics = "Eastern Frisian" }
                                  , defaultConstructor { constructorName = "FRY", constructorSemantics = "Western Frisian" }
                                  , defaultConstructor { constructorName = "FUL", constructorSemantics = "Fulah" }
                                  , defaultConstructor { constructorName = "FUR", constructorSemantics = "Friulian" }
                                  , defaultConstructor { constructorName = "GAA", constructorSemantics = "Ga" }
                                  , defaultConstructor { constructorName = "GAY", constructorSemantics = "Gayo" }
                                  , defaultConstructor { constructorName = "GBA", constructorSemantics = "Gbaya" }
                                  , defaultConstructor { constructorName = "GEM", constructorSemantics = "Germanic languages" }
                                  , defaultConstructor { constructorName = "GEZ", constructorSemantics = "Geez" }
                                  , defaultConstructor { constructorName = "GIL", constructorSemantics = "Gilbertese" }
                                  , defaultConstructor { constructorName = "GLA", constructorSemantics = "Gaelic; Scottish Gaelic" }
                                  , defaultConstructor { constructorName = "GLE", constructorSemantics = "Irish" }
                                  , defaultConstructor { constructorName = "GLG", constructorSemantics = "Galician" }
                                  , defaultConstructor { constructorName = "GLV", constructorSemantics = "Manx" }
                                  , defaultConstructor { constructorName = "GMH", constructorSemantics = "German, Middle High (ca.1050-1500)" }
                                  , defaultConstructor { constructorName = "GOH", constructorSemantics = "German, Old High (ca.750-1050)" }
                                  , defaultConstructor { constructorName = "GON", constructorSemantics = "Gondi" }
                                  , defaultConstructor { constructorName = "GOR", constructorSemantics = "Gorontalo" }
                                  , defaultConstructor { constructorName = "GOT", constructorSemantics = "Gothic" }
                                  , defaultConstructor { constructorName = "GRB", constructorSemantics = "Grebo" }
                                  , defaultConstructor { constructorName = "GRC", constructorSemantics = "Greek, Ancient (to 1453)" }
                                  , defaultConstructor { constructorName = "GRN", constructorSemantics = "Guarani" }
                                  , defaultConstructor { constructorName = "GSW", constructorSemantics = "Swiss German; Alemannic; Alsatian" }
                                  , defaultConstructor { constructorName = "GUJ", constructorSemantics = "Gujarati" }
                                  , defaultConstructor { constructorName = "GWI", constructorSemantics = "Gwich'in" }
                                  , defaultConstructor { constructorName = "HAI", constructorSemantics = "Haida" }
                                  , defaultConstructor { constructorName = "HAT", constructorSemantics = "Haitian; Haitian Creole" }
                                  , defaultConstructor { constructorName = "HAU", constructorSemantics = "Hausa" }
                                  , defaultConstructor { constructorName = "HAW", constructorSemantics = "Hawaiian" }
                                  , defaultConstructor { constructorName = "HEB", constructorSemantics = "Hebrew" }
                                  , defaultConstructor { constructorName = "HER", constructorSemantics = "Herero" }
                                  , defaultConstructor { constructorName = "HIL", constructorSemantics = "Hiligaynon" }
                                  , defaultConstructor { constructorName = "HIM", constructorSemantics = "Himachali languages; Western Pahari languages" }
                                  , defaultConstructor { constructorName = "HIN", constructorSemantics = "Hindi" }
                                  , defaultConstructor { constructorName = "HIT", constructorSemantics = "Hittite" }
                                  , defaultConstructor { constructorName = "HMN", constructorSemantics = "Hmong; Mong" }
                                  , defaultConstructor { constructorName = "HMO", constructorSemantics = "Hiri Motu" }
                                  , defaultConstructor { constructorName = "HRV", constructorSemantics = "Croatian" }
                                  , defaultConstructor { constructorName = "HSB", constructorSemantics = "Upper Sorbian" }
                                  , defaultConstructor { constructorName = "HUN", constructorSemantics = "Hungarian" }
                                  , defaultConstructor { constructorName = "HUP", constructorSemantics = "Hupa" }
                                  , defaultConstructor { constructorName = "HYE", constructorSemantics = "Armenian" }
                                  , defaultConstructor { constructorName = "IBA", constructorSemantics = "Iban" }
                                  , defaultConstructor { constructorName = "IBO", constructorSemantics = "Igbo" }
                                  , defaultConstructor { constructorName = "IDO", constructorSemantics = "Ido" }
                                  , defaultConstructor { constructorName = "III", constructorSemantics = "Sichuan Yi; Nuosu" }
                                  , defaultConstructor { constructorName = "IJO", constructorSemantics = "Ijo languages" }
                                  , defaultConstructor { constructorName = "IKU", constructorSemantics = "Inuktitut" }
                                  , defaultConstructor { constructorName = "ILE", constructorSemantics = "Interlingue; Occidental" }
                                  , defaultConstructor { constructorName = "ILO", constructorSemantics = "Iloko" }
                                  , defaultConstructor { constructorName = "INA", constructorSemantics = "Interlingua (International Auxiliary Language Association)" }
                                  , defaultConstructor { constructorName = "INC", constructorSemantics = "Indic languages" }
                                  , defaultConstructor { constructorName = "IND", constructorSemantics = "Indonesian" }
                                  , defaultConstructor { constructorName = "INE", constructorSemantics = "Indo-European languages" }
                                  , defaultConstructor { constructorName = "INH", constructorSemantics = "Ingush" }
                                  , defaultConstructor { constructorName = "IPK", constructorSemantics = "Inupiaq" }
                                  , defaultConstructor { constructorName = "IRA", constructorSemantics = "Iranian languages" }
                                  , defaultConstructor { constructorName = "IRO", constructorSemantics = "Iroquoian languages" }
                                  , defaultConstructor { constructorName = "ISL", constructorSemantics = "Icelandic" }
                                  , defaultConstructor { constructorName = "ITA", constructorSemantics = "Italian" }
                                  , defaultConstructor { constructorName = "JAV", constructorSemantics = "Javanese" }
                                  , defaultConstructor { constructorName = "JBO", constructorSemantics = "Lojban" }
                                  , defaultConstructor { constructorName = "JPN", constructorSemantics = "Japanese" }
                                  , defaultConstructor { constructorName = "JPR", constructorSemantics = "Judeo-Persian" }
                                  , defaultConstructor { constructorName = "JRB", constructorSemantics = "Judeo-Arabic" }
                                  , defaultConstructor { constructorName = "KAA", constructorSemantics = "Kara-Kalpak" }
                                  , defaultConstructor { constructorName = "KAB", constructorSemantics = "Kabyle" }
                                  , defaultConstructor { constructorName = "KAC", constructorSemantics = "Kachin; Jingpho" }
                                  , defaultConstructor { constructorName = "KAL", constructorSemantics = "Kalaallisut; Greenlandic" }
                                  , defaultConstructor { constructorName = "KAM", constructorSemantics = "Kamba" }
                                  , defaultConstructor { constructorName = "KAN", constructorSemantics = "Kannada" }
                                  , defaultConstructor { constructorName = "KAR", constructorSemantics = "Karen languages" }
                                  , defaultConstructor { constructorName = "KAS", constructorSemantics = "Kashmiri" }
                                  , defaultConstructor { constructorName = "KAT", constructorSemantics = "Georgian" }
                                  , defaultConstructor { constructorName = "KAU", constructorSemantics = "Kanuri" }
                                  , defaultConstructor { constructorName = "KAW", constructorSemantics = "Kawi" }
                                  , defaultConstructor { constructorName = "KAZ", constructorSemantics = "Kazakh" }
                                  , defaultConstructor { constructorName = "KBD", constructorSemantics = "Kabardian" }
                                  , defaultConstructor { constructorName = "KHA", constructorSemantics = "Khasi" }
                                  , defaultConstructor { constructorName = "KHI", constructorSemantics = "Khoisan languages" }
                                  , defaultConstructor { constructorName = "KHM", constructorSemantics = "Central Khmer" }
                                  , defaultConstructor { constructorName = "KHO", constructorSemantics = "Khotanese; Sakan" }
                                  , defaultConstructor { constructorName = "KIK", constructorSemantics = "Kikuyu; Gikuyu" }
                                  , defaultConstructor { constructorName = "KIN", constructorSemantics = "Kinyarwanda" }
                                  , defaultConstructor { constructorName = "KIR", constructorSemantics = "Kirghiz; Kyrgyz" }
                                  , defaultConstructor { constructorName = "KMB", constructorSemantics = "Kimbundu" }
                                  , defaultConstructor { constructorName = "KOK", constructorSemantics = "Konkani" }
                                  , defaultConstructor { constructorName = "KOM", constructorSemantics = "Komi" }
                                  , defaultConstructor { constructorName = "KON", constructorSemantics = "Kongo" }
                                  , defaultConstructor { constructorName = "KOR", constructorSemantics = "Korean" }
                                  , defaultConstructor { constructorName = "KOS", constructorSemantics = "Kosraean" }
                                  , defaultConstructor { constructorName = "KPE", constructorSemantics = "Kpelle" }
                                  , defaultConstructor { constructorName = "KRC", constructorSemantics = "Karachay-Balkar" }
                                  , defaultConstructor { constructorName = "KRL", constructorSemantics = "Karelian" }
                                  , defaultConstructor { constructorName = "KRO", constructorSemantics = "Kru languages" }
                                  , defaultConstructor { constructorName = "KRU", constructorSemantics = "Kurukh" }
                                  , defaultConstructor { constructorName = "KUA", constructorSemantics = "Kuanyama; Kwanyama" }
                                  , defaultConstructor { constructorName = "KUM", constructorSemantics = "Kumyk" }
                                  , defaultConstructor { constructorName = "KUR", constructorSemantics = "Kurdish" }
                                  , defaultConstructor { constructorName = "KUT", constructorSemantics = "Kutenai" }
                                  , defaultConstructor { constructorName = "LAD", constructorSemantics = "Ladino" }
                                  , defaultConstructor { constructorName = "LAH", constructorSemantics = "Lahnda" }
                                  , defaultConstructor { constructorName = "LAM", constructorSemantics = "Lamba" }
                                  , defaultConstructor { constructorName = "LAO", constructorSemantics = "Lao" }
                                  , defaultConstructor { constructorName = "LAT", constructorSemantics = "Latin" }
                                  , defaultConstructor { constructorName = "LAV", constructorSemantics = "Latvian" }
                                  , defaultConstructor { constructorName = "LEZ", constructorSemantics = "Lezghian" }
                                  , defaultConstructor { constructorName = "LIM", constructorSemantics = "Limburgan; Limburger; Limburgish" }
                                  , defaultConstructor { constructorName = "LIN", constructorSemantics = "Lingala" }
                                  , defaultConstructor { constructorName = "LIT", constructorSemantics = "Lithuanian" }
                                  , defaultConstructor { constructorName = "LOL", constructorSemantics = "Mongo" }
                                  , defaultConstructor { constructorName = "LOZ", constructorSemantics = "Lozi" }
                                  , defaultConstructor { constructorName = "LTZ", constructorSemantics = "Luxembourgish; Letzeburgesch" }
                                  , defaultConstructor { constructorName = "LUA", constructorSemantics = "Luba-Lulua" }
                                  , defaultConstructor { constructorName = "LUB", constructorSemantics = "Luba-Katanga" }
                                  , defaultConstructor { constructorName = "LUG", constructorSemantics = "Ganda" }
                                  , defaultConstructor { constructorName = "LUI", constructorSemantics = "Luiseno" }
                                  , defaultConstructor { constructorName = "LUN", constructorSemantics = "Lunda" }
                                  , defaultConstructor { constructorName = "LUO", constructorSemantics = "Luo (Kenya and Tanzania)" }
                                  , defaultConstructor { constructorName = "LUS", constructorSemantics = "Lushai" }
                                  , defaultConstructor { constructorName = "MAD", constructorSemantics = "Madurese" }
                                  , defaultConstructor { constructorName = "MAG", constructorSemantics = "Magahi" }
                                  , defaultConstructor { constructorName = "MAH", constructorSemantics = "Marshallese" }
                                  , defaultConstructor { constructorName = "MAI", constructorSemantics = "Maithili" }
                                  , defaultConstructor { constructorName = "MAK", constructorSemantics = "Makasar" }
                                  , defaultConstructor { constructorName = "MAL", constructorSemantics = "Malayalam" }
                                  , defaultConstructor { constructorName = "MAN", constructorSemantics = "Mandingo" }
                                  , defaultConstructor { constructorName = "MAP", constructorSemantics = "Austronesian languages" }
                                  , defaultConstructor { constructorName = "MAR", constructorSemantics = "Marathi" }
                                  , defaultConstructor { constructorName = "MAS", constructorSemantics = "Masai" }
                                  , defaultConstructor { constructorName = "MDF", constructorSemantics = "Moksha" }
                                  , defaultConstructor { constructorName = "MDR", constructorSemantics = "Mandar" }
                                  , defaultConstructor { constructorName = "MEN", constructorSemantics = "Mende" }
                                  , defaultConstructor { constructorName = "MGA", constructorSemantics = "Irish, Middle (900-1200)" }
                                  , defaultConstructor { constructorName = "MIC", constructorSemantics = "Mi'kmaq; Micmac" }
                                  , defaultConstructor { constructorName = "MIN", constructorSemantics = "Minangkabau" }
                                  , defaultConstructor { constructorName = "MIS", constructorSemantics = "Uncoded languages" }
                                  , defaultConstructor { constructorName = "MKD", constructorSemantics = "Macedonian" }
                                  , defaultConstructor { constructorName = "MKH", constructorSemantics = "Mon-Khmer languages" }
                                  , defaultConstructor { constructorName = "MLG", constructorSemantics = "Malagasy" }
                                  , defaultConstructor { constructorName = "MLT", constructorSemantics = "Maltese" }
                                  , defaultConstructor { constructorName = "MNC", constructorSemantics = "Manchu" }
                                  , defaultConstructor { constructorName = "MNI", constructorSemantics = "Manipuri" }
                                  , defaultConstructor { constructorName = "MNO", constructorSemantics = "Manobo languages" }
                                  , defaultConstructor { constructorName = "MOH", constructorSemantics = "Mohawk" }
                                  , defaultConstructor { constructorName = "MON", constructorSemantics = "Mongolian" }
                                  , defaultConstructor { constructorName = "MOS", constructorSemantics = "Mossi" }
                                  , defaultConstructor { constructorName = "MRI", constructorSemantics = "Maori" }
                                  , defaultConstructor { constructorName = "MSA", constructorSemantics = "Malay" }
                                  , defaultConstructor { constructorName = "MUL", constructorSemantics = "Multiple languages" }
                                  , defaultConstructor { constructorName = "MUN", constructorSemantics = "Munda languages" }
                                  , defaultConstructor { constructorName = "MUS", constructorSemantics = "Creek" }
                                  , defaultConstructor { constructorName = "MWL", constructorSemantics = "Mirandese" }
                                  , defaultConstructor { constructorName = "MWR", constructorSemantics = "Marwari" }
                                  , defaultConstructor { constructorName = "MYA", constructorSemantics = "Burmese" }
                                  , defaultConstructor { constructorName = "MYN", constructorSemantics = "Mayan languages" }
                                  , defaultConstructor { constructorName = "MYV", constructorSemantics = "Erzya" }
                                  , defaultConstructor { constructorName = "NAH", constructorSemantics = "Nahuatl languages" }
                                  , defaultConstructor { constructorName = "NAI", constructorSemantics = "North American Indian languages" }
                                  , defaultConstructor { constructorName = "NAP", constructorSemantics = "Neapolitan" }
                                  , defaultConstructor { constructorName = "NAU", constructorSemantics = "Nauru" }
                                  , defaultConstructor { constructorName = "NAV", constructorSemantics = "Navajo; Navaho" }
                                  , defaultConstructor { constructorName = "NBL", constructorSemantics = "Ndebele, South; South Ndebele" }
                                  , defaultConstructor { constructorName = "NDE", constructorSemantics = "Ndebele, North; North Ndebele" }
                                  , defaultConstructor { constructorName = "NDO", constructorSemantics = "Ndonga" }
                                  , defaultConstructor { constructorName = "NDS", constructorSemantics = "Low German; Low Saxon; German, Low; Saxon, Low" }
                                  , defaultConstructor { constructorName = "NEP", constructorSemantics = "Nepali" }
                                  , defaultConstructor { constructorName = "NEW", constructorSemantics = "Nepal Bhasa; Newari" }
                                  , defaultConstructor { constructorName = "NIA", constructorSemantics = "Nias" }
                                  , defaultConstructor { constructorName = "NIC", constructorSemantics = "Niger-Kordofanian languages" }
                                  , defaultConstructor { constructorName = "NIU", constructorSemantics = "Niuean" }
                                  , defaultConstructor { constructorName = "NLD", constructorSemantics = "Dutch; Flemish" }
                                  , defaultConstructor { constructorName = "NNO", constructorSemantics = "Norwegian Nynorsk; Nynorsk, Norwegian" }
                                  , defaultConstructor { constructorName = "NOB", constructorSemantics = "Bokmål, Norwegian; Norwegian Bokmål" }
                                  , defaultConstructor { constructorName = "NOG", constructorSemantics = "Nogai" }
                                  , defaultConstructor { constructorName = "NON", constructorSemantics = "Norse, Old" }
                                  , defaultConstructor { constructorName = "NOR", constructorSemantics = "Norwegian" }
                                  , defaultConstructor { constructorName = "NQO", constructorSemantics = "N'Ko" }
                                  , defaultConstructor { constructorName = "NSO", constructorSemantics = "Pedi; Sepedi; Northern Sotho" }
                                  , defaultConstructor { constructorName = "NUB", constructorSemantics = "Nubian languages" }
                                  , defaultConstructor { constructorName = "NWC", constructorSemantics = "Classical Newari; Old Newari; Classical Nepal Bhasa" }
                                  , defaultConstructor { constructorName = "NYA", constructorSemantics = "Chichewa; Chewa; Nyanja" }
                                  , defaultConstructor { constructorName = "NYM", constructorSemantics = "Nyamwezi" }
                                  , defaultConstructor { constructorName = "NYN", constructorSemantics = "Nyankole" }
                                  , defaultConstructor { constructorName = "NYO", constructorSemantics = "Nyoro" }
                                  , defaultConstructor { constructorName = "NZI", constructorSemantics = "Nzima" }
                                  , defaultConstructor { constructorName = "OCI", constructorSemantics = "Occitan (post 1500)" }
                                  , defaultConstructor { constructorName = "OJI", constructorSemantics = "Ojibwa" }
                                  , defaultConstructor { constructorName = "ORI", constructorSemantics = "Oriya" }
                                  , defaultConstructor { constructorName = "ORM", constructorSemantics = "Oromo" }
                                  , defaultConstructor { constructorName = "OSA", constructorSemantics = "Osage" }
                                  , defaultConstructor { constructorName = "OSS", constructorSemantics = "Ossetian; Ossetic" }
                                  , defaultConstructor { constructorName = "OTA", constructorSemantics = "Turkish, Ottoman (1500-1928)" }
                                  , defaultConstructor { constructorName = "OTO", constructorSemantics = "Otomian languages" }
                                  , defaultConstructor { constructorName = "PAA", constructorSemantics = "Papuan languages" }
                                  , defaultConstructor { constructorName = "PAG", constructorSemantics = "Pangasinan" }
                                  , defaultConstructor { constructorName = "PAL", constructorSemantics = "Pahlavi" }
                                  , defaultConstructor { constructorName = "PAM", constructorSemantics = "Pampanga; Kapampangan" }
                                  , defaultConstructor { constructorName = "PAN", constructorSemantics = "Panjabi; Punjabi" }
                                  , defaultConstructor { constructorName = "PAP", constructorSemantics = "Papiamento" }
                                  , defaultConstructor { constructorName = "PAU", constructorSemantics = "Palauan" }
                                  , defaultConstructor { constructorName = "PEO", constructorSemantics = "Persian, Old (ca.600-400 B.C.)" }
                                  , defaultConstructor { constructorName = "PHI", constructorSemantics = "Philippine languages" }
                                  , defaultConstructor { constructorName = "PHN", constructorSemantics = "Phoenician" }
                                  , defaultConstructor { constructorName = "PLI", constructorSemantics = "Pali" }
                                  , defaultConstructor { constructorName = "POL", constructorSemantics = "Polish" }
                                  , defaultConstructor { constructorName = "PON", constructorSemantics = "Pohnpeian" }
                                  , defaultConstructor { constructorName = "POR", constructorSemantics = "Portuguese" }
                                  , defaultConstructor { constructorName = "PRA", constructorSemantics = "Prakrit languages" }
                                  , defaultConstructor { constructorName = "PRO", constructorSemantics = "Provençal, Old (to 1500);Occitan, Old (to 1500)" }
                                  , defaultConstructor { constructorName = "PUS", constructorSemantics = "Pushto; Pashto" }
                                  , defaultConstructor { constructorName = "QAA", constructorSemantics = "QTZ Reserved for local use" }
                                  , defaultConstructor { constructorName = "QUE", constructorSemantics = "Quechua" }
                                  , defaultConstructor { constructorName = "RAJ", constructorSemantics = "Rajasthani" }
                                  , defaultConstructor { constructorName = "RAP", constructorSemantics = "Rapanui" }
                                  , defaultConstructor { constructorName = "RAR", constructorSemantics = "Rarotongan; Cook Islands Maori" }
                                  , defaultConstructor { constructorName = "ROA", constructorSemantics = "Romance languages" }
                                  , defaultConstructor { constructorName = "ROH", constructorSemantics = "Romansh" }
                                  , defaultConstructor { constructorName = "ROM", constructorSemantics = "Romany" }
                                  , defaultConstructor { constructorName = "RON", constructorSemantics = "Romanian; Moldavian; Moldovan" }
                                  , defaultConstructor { constructorName = "RUN", constructorSemantics = "Rundi" }
                                  , defaultConstructor { constructorName = "RUP", constructorSemantics = "Aromanian; Arumanian; Macedo-Romanian" }
                                  , defaultConstructor { constructorName = "RUS", constructorSemantics = "Russian" }
                                  , defaultConstructor { constructorName = "SAD", constructorSemantics = "Sandawe" }
                                  , defaultConstructor { constructorName = "SAG", constructorSemantics = "Sango" }
                                  , defaultConstructor { constructorName = "SAH", constructorSemantics = "Yakut" }
                                  , defaultConstructor { constructorName = "SAI", constructorSemantics = "South American Indian languages" }
                                  , defaultConstructor { constructorName = "SAL", constructorSemantics = "Salishan languages" }
                                  , defaultConstructor { constructorName = "SAM", constructorSemantics = "Samaritan Aramaic" }
                                  , defaultConstructor { constructorName = "SAN", constructorSemantics = "Sanskrit" }
                                  , defaultConstructor { constructorName = "SAS", constructorSemantics = "Sasak" }
                                  , defaultConstructor { constructorName = "SAT", constructorSemantics = "Santali" }
                                  , defaultConstructor { constructorName = "SCN", constructorSemantics = "Sicilian" }
                                  , defaultConstructor { constructorName = "SCO", constructorSemantics = "Scots" }
                                  , defaultConstructor { constructorName = "SEL", constructorSemantics = "Selkup" }
                                  , defaultConstructor { constructorName = "SEM", constructorSemantics = "Semitic languages" }
                                  , defaultConstructor { constructorName = "SGA", constructorSemantics = "Irish, Old (to 900)" }
                                  , defaultConstructor { constructorName = "SGN", constructorSemantics = "Sign Languages" }
                                  , defaultConstructor { constructorName = "SHN", constructorSemantics = "Shan" }
                                  , defaultConstructor { constructorName = "SID", constructorSemantics = "Sidamo" }
                                  , defaultConstructor { constructorName = "SIN", constructorSemantics = "Sinhala; Sinhalese" }
                                  , defaultConstructor { constructorName = "SIO", constructorSemantics = "Siouan languages" }
                                  , defaultConstructor { constructorName = "SIT", constructorSemantics = "Sino-Tibetan languages" }
                                  , defaultConstructor { constructorName = "SLA", constructorSemantics = "Slavic languages" }
                                  , defaultConstructor { constructorName = "SLK", constructorSemantics = "Slovak" }
                                  , defaultConstructor { constructorName = "SLV", constructorSemantics = "Slovenian" }
                                  , defaultConstructor { constructorName = "SMA", constructorSemantics = "Southern Sami" }
                                  , defaultConstructor { constructorName = "SME", constructorSemantics = "Northern Sami" }
                                  , defaultConstructor { constructorName = "SMI", constructorSemantics = "Sami languages" }
                                  , defaultConstructor { constructorName = "SMJ", constructorSemantics = "Lule Sami" }
                                  , defaultConstructor { constructorName = "SMN", constructorSemantics = "Inari Sami" }
                                  , defaultConstructor { constructorName = "SMO", constructorSemantics = "Samoan" }
                                  , defaultConstructor { constructorName = "SMS", constructorSemantics = "Skolt Sami" }
                                  , defaultConstructor { constructorName = "SNA", constructorSemantics = "Shona" }
                                  , defaultConstructor { constructorName = "SND", constructorSemantics = "Sindhi" }
                                  , defaultConstructor { constructorName = "SNK", constructorSemantics = "Soninke" }
                                  , defaultConstructor { constructorName = "SOG", constructorSemantics = "Sogdian" }
                                  , defaultConstructor { constructorName = "SOM", constructorSemantics = "Somali" }
                                  , defaultConstructor { constructorName = "SON", constructorSemantics = "Songhai languages" }
                                  , defaultConstructor { constructorName = "SOT", constructorSemantics = "Sotho, Southern" }
                                  , defaultConstructor { constructorName = "SPA", constructorSemantics = "Spanish; Castilian" }
                                  , defaultConstructor { constructorName = "SQI", constructorSemantics = "Albanian" }
                                  , defaultConstructor { constructorName = "SRD", constructorSemantics = "Sardinian" }
                                  , defaultConstructor { constructorName = "SRN", constructorSemantics = "Sranan Tongo" }
                                  , defaultConstructor { constructorName = "SRP", constructorSemantics = "Serbian" }
                                  , defaultConstructor { constructorName = "SRR", constructorSemantics = "Serer" }
                                  , defaultConstructor { constructorName = "SSA", constructorSemantics = "Nilo-Saharan languages" }
                                  , defaultConstructor { constructorName = "SSW", constructorSemantics = "Swati" }
                                  , defaultConstructor { constructorName = "SUK", constructorSemantics = "Sukuma" }
                                  , defaultConstructor { constructorName = "SUN", constructorSemantics = "Sundanese" }
                                  , defaultConstructor { constructorName = "SUS", constructorSemantics = "Susu" }
                                  , defaultConstructor { constructorName = "SUX", constructorSemantics = "Sumerian" }
                                  , defaultConstructor { constructorName = "SWA", constructorSemantics = "Swahili" }
                                  , defaultConstructor { constructorName = "SWE", constructorSemantics = "Swedish" }
                                  , defaultConstructor { constructorName = "SYC", constructorSemantics = "Classical Syriac" }
                                  , defaultConstructor { constructorName = "SYR", constructorSemantics = "Syriac" }
                                  , defaultConstructor { constructorName = "TAH", constructorSemantics = "Tahitian" }
                                  , defaultConstructor { constructorName = "TAI", constructorSemantics = "Tai languages" }
                                  , defaultConstructor { constructorName = "TAM", constructorSemantics = "Tamil" }
                                  , defaultConstructor { constructorName = "TAT", constructorSemantics = "Tatar" }
                                  , defaultConstructor { constructorName = "TEL", constructorSemantics = "Telugu" }
                                  , defaultConstructor { constructorName = "TEM", constructorSemantics = "Timne" }
                                  , defaultConstructor { constructorName = "TER", constructorSemantics = "Tereno" }
                                  , defaultConstructor { constructorName = "TET", constructorSemantics = "Tetum" }
                                  , defaultConstructor { constructorName = "TGK", constructorSemantics = "Tajik" }
                                  , defaultConstructor { constructorName = "TGL", constructorSemantics = "Tagalog" }
                                  , defaultConstructor { constructorName = "THA", constructorSemantics = "Thai" }
                                  , defaultConstructor { constructorName = "TIG", constructorSemantics = "Tigre" }
                                  , defaultConstructor { constructorName = "TIR", constructorSemantics = "Tigrinya" }
                                  , defaultConstructor { constructorName = "TIV", constructorSemantics = "Tiv" }
                                  , defaultConstructor { constructorName = "TKL", constructorSemantics = "Tokelau" }
                                  , defaultConstructor { constructorName = "TLH", constructorSemantics = "Klingon; tlhIngan-Hol" }
                                  , defaultConstructor { constructorName = "TLI", constructorSemantics = "Tlingit" }
                                  , defaultConstructor { constructorName = "TMH", constructorSemantics = "Tamashek" }
                                  , defaultConstructor { constructorName = "TOG", constructorSemantics = "Tonga (Nyasa)" }
                                  , defaultConstructor { constructorName = "TON", constructorSemantics = "Tonga (Tonga Islands)" }
                                  , defaultConstructor { constructorName = "TPI", constructorSemantics = "Tok Pisin" }
                                  , defaultConstructor { constructorName = "TSI", constructorSemantics = "Tsimshian" }
                                  , defaultConstructor { constructorName = "TSN", constructorSemantics = "Tswana" }
                                  , defaultConstructor { constructorName = "TSO", constructorSemantics = "Tsonga" }
                                  , defaultConstructor { constructorName = "TUK", constructorSemantics = "Turkmen" }
                                  , defaultConstructor { constructorName = "TUM", constructorSemantics = "Tumbuka" }
                                  , defaultConstructor { constructorName = "TUP", constructorSemantics = "Tupi languages" }
                                  , defaultConstructor { constructorName = "TUR", constructorSemantics = "Turkish" }
                                  , defaultConstructor { constructorName = "TUT", constructorSemantics = "Altaic languages" }
                                  , defaultConstructor { constructorName = "TVL", constructorSemantics = "Tuvalu" }
                                  , defaultConstructor { constructorName = "TWI", constructorSemantics = "Twi" }
                                  , defaultConstructor { constructorName = "TYV", constructorSemantics = "Tuvinian" }
                                  , defaultConstructor { constructorName = "UDM", constructorSemantics = "Udmurt" }
                                  , defaultConstructor { constructorName = "UGA", constructorSemantics = "Ugaritic" }
                                  , defaultConstructor { constructorName = "UIG", constructorSemantics = "Uighur; Uyghur" }
                                  , defaultConstructor { constructorName = "UKR", constructorSemantics = "Ukrainian" }
                                  , defaultConstructor { constructorName = "UMB", constructorSemantics = "Umbundu" }
                                  , defaultConstructor { constructorName = "UND", constructorSemantics = "Undetermined" }
                                  , defaultConstructor { constructorName = "URD", constructorSemantics = "Urdu" }
                                  , defaultConstructor { constructorName = "UZB", constructorSemantics = "Uzbek" }
                                  , defaultConstructor { constructorName = "VAI", constructorSemantics = "Vai" }
                                  , defaultConstructor { constructorName = "VEN", constructorSemantics = "Venda" }
                                  , defaultConstructor { constructorName = "VIE", constructorSemantics = "Vietnamese" }
                                  , defaultConstructor { constructorName = "VOL", constructorSemantics = "Volapük" }
                                  , defaultConstructor { constructorName = "VOT", constructorSemantics = "Votic" }
                                  , defaultConstructor { constructorName = "WAK", constructorSemantics = "Wakashan languages" }
                                  , defaultConstructor { constructorName = "WAL", constructorSemantics = "Wolaitta; Wolaytta" }
                                  , defaultConstructor { constructorName = "WAR", constructorSemantics = "Waray" }
                                  , defaultConstructor { constructorName = "WAS", constructorSemantics = "Washo" }
                                  , defaultConstructor { constructorName = "WEN", constructorSemantics = "Sorbian languages" }
                                  , defaultConstructor { constructorName = "WLN", constructorSemantics = "Walloon" }
                                  , defaultConstructor { constructorName = "WOL", constructorSemantics = "Wolof" }
                                  , defaultConstructor { constructorName = "XAL", constructorSemantics = "Kalmyk; Oirat" }
                                  , defaultConstructor { constructorName = "XHO", constructorSemantics = "Xhosa" }
                                  , defaultConstructor { constructorName = "YAO", constructorSemantics = "Yao" }
                                  , defaultConstructor { constructorName = "YAP", constructorSemantics = "Yapese" }
                                  , defaultConstructor { constructorName = "YID", constructorSemantics = "Yiddish" }
                                  , defaultConstructor { constructorName = "YOR", constructorSemantics = "Yoruba" }
                                  , defaultConstructor { constructorName = "YPK", constructorSemantics = "Yupik languages" }
                                  , defaultConstructor { constructorName = "ZAP", constructorSemantics = "Zapotec" }
                                  , defaultConstructor { constructorName = "ZBL", constructorSemantics = "Blissymbols; Blissymbolics; Bliss" }
                                  , defaultConstructor { constructorName = "ZEN", constructorSemantics = "Zenaga" }
                                  , defaultConstructor { constructorName = "ZHA", constructorSemantics = "Zhuang; Chuang" }
                                  , defaultConstructor { constructorName = "ZHO", constructorSemantics = "Chinese" }
                                  , defaultConstructor { constructorName = "ZND", constructorSemantics = "Zande languages" }
                                  , defaultConstructor { constructorName = "ZUL", constructorSemantics = "Zulu" }
                                  , defaultConstructor { constructorName = "ZUN", constructorSemantics = "Zuni" }
                                  , defaultConstructor { constructorName = "ZXX", constructorSemantics = "No linguistic content; Not applicable" }
                                  , defaultConstructor { constructorName = "ZZA", constructorSemantics = "Zaza; Dimili; Dimli; Kirdki; Kirmanjki; Zazaki" }
                                 ]
          }               


t59     :: TypeDefinition Concrete 
t59 = defaultType {
	identifier = "90eceef9-1189-4a18-903b-9cf36eb18e97",
  author       = Just personMikael,
	name = "Permission",
	semantics = "File access permission mask.",
	constructors = Just [
		defaultConstructor {
			constructorName = "Permission",
			constructorFields = [
				(defaultField :: Field Concrete) {
					fieldName = "read",
					fieldSemantics = "Read permission.",
					fieldType = DataType "0219c59f732a8ef507215fbdb4cceacd"
				},
				(defaultField :: Field Concrete) {
					fieldName = "write",
					fieldSemantics = "Write permission.",
					fieldType = DataType "0219c59f732a8ef507215fbdb4cceacd"
				},
				(defaultField :: Field Concrete) {
					fieldName = "exec",
					fieldSemantics = "Execute permission.",
					fieldType = DataType "0219c59f732a8ef507215fbdb4cceacd"
				}
			]
		}
	]
}

t60     :: TypeDefinition Concrete 
t60 = defaultType {
	identifier = "add67dbc-2e18-4ffd-aea3-b1e8cb28f7d8",
  author       = Just personMikael,
	name = "UnixTime",
	semantics = "UNIX time stamp.",
	constructors = Just [
		defaultConstructor {
			constructorName = "UnixTime",
			constructorFields = [
				(defaultField :: Field Concrete) {
					fieldName = "nanoSeconds",
					fieldSemantics = "Nanoseconds since the Epoch, 1970-01-01 00:00 +0000.",
					fieldType = DataType "ac2e770f2132aced749ec197385ff552"
				}
			]
		}
	]
}

t61     :: TypeDefinition Concrete 
t61 = defaultType {
	identifier = "41f3d3c6-311b-4f18-a600-758219595871",
  author       = Just personMikael,
	name = "SpecialType",
	semantics = "Special file type.",
	constructors = Just [
		defaultConstructor { constructorName = "Character" },
		defaultConstructor { constructorName = "Block" }
	]
}

t62     :: TypeDefinition Concrete 
t62 = defaultType {
	identifier = "fa052506-7ac8-4473-a274-c4bac5ad0cc4",
  author       = Just personMikael,
	name = "IPCType",
	semantics = "IPC object type.",
	constructors = Just [
		defaultConstructor { constructorName = "Pipe" },
		defaultConstructor { constructorName = "Socket" }
	]
}

t63     :: TypeDefinition (Application Concrete Concrete)
t63 = defaultType {
	identifier = "027770dd-5134-4ee0-8cd8-faf29e962167",
  author       = Just personMikael,
	name = "File",
	semantics = "File system file.",
	constructors = Just [
		(defaultConstructor :: Constructor (Application Concrete Concrete)) {
			constructorName = "Directory",
			constructorSemantics = "File directory.",
			constructorFields = [
				(defaultField :: Field  (Application Concrete Concrete)) {
					fieldName = "defacl",
					fieldSemantics = "Default permission mask for access control lists.",
					fieldType = DataType "90eceef9-1189-4a18-903b-9cf36eb18e97"
				},
				(defaultField :: Field  (Application Concrete Concrete)) {
					fieldName = "entries",
					fieldSemantics = "Directory entries.",
					fieldType =
						(Application
							(Application
								(DataType "43c6cd13-33b0-4fc8-a480-668ecb24768e")
								(DataType "4f7db06c439541658a09689d3e7dd909")
							)
							(Application
								(DataType "4eabdb36-0fbf-4df6-9dcd-33d16dac9516")
								(Variable First)
							)
						)
				}
			]
		},
		(defaultConstructor :: Constructor (Application Concrete Concrete)) {
			constructorName = "Link",
			constructorSemantics = "Symbolic link.",
			constructorFields = [
				(defaultField :: Field  (Application Concrete Concrete)) {
					fieldName = "target",
					fieldSemantics = "Link target.",
					fieldType = DataType "5448c6b7-9a08-4b4e-a40d-442c4fd2e125"
				}
			]
		},
		(defaultConstructor :: Constructor (Application Concrete Concrete)) {
			constructorName = "Special",
			constructorSemantics = "Special (device) file.",
			constructorFields = [
				(defaultField :: Field  (Application Concrete Concrete)) {
					fieldName = "specialType",
					fieldSemantics = "Type of special file.",
					fieldType = DataType "41f3d3c6-311b-4f18-a600-758219595871"
				},
				(defaultField :: Field  (Application Concrete Concrete)) {
					fieldName = "major",
					fieldSemantics = "Major device number.",
					fieldType = DataType "62d2d537-1f08-461a-a328-bc06561594f6"
				},
				(defaultField :: Field  (Application Concrete Concrete)) {
					fieldName = "minor",
					fieldSemantics = "Minor device number.",
					fieldType = DataType "62d2d537-1f08-461a-a328-bc06561594f6"
				}
			]
		},
		(defaultConstructor :: Constructor (Application Concrete Concrete)) {
			constructorName = "IPC",
			constructorSemantics = "Inter-process communication object.",
			constructorFields = [
				(defaultField :: Field  (Application Concrete Concrete)){
					fieldName = "ipcType",
					fieldSemantics = "Type of IPC object.",
					fieldType = DataType "fa052506-7ac8-4473-a274-c4bac5ad0cc4"
				}
			]
		},
		(defaultConstructor :: Constructor  (Application Concrete Concrete)) {
			constructorName = "Regular",
			constructorSemantics = "Regular file.",
			constructorFields = [
				(defaultField :: Field  (Application Concrete Concrete)) {
					fieldName = "content",
					fieldSemantics = "File content.",
					fieldType = DataType "f9f2f27af0f649b4bc8946c467c3b76a"
				}
			]
		}
	]
}

t64     :: TypeDefinition (Application Concrete Concrete) 
t64 = defaultType {
	identifier = "4eabdb36-0fbf-4df6-9dcd-33d16dac9516",
  author       = Just personMikael,
	name = "Inode",
	semantics = "File system inode.",
	constructors = Just [
		defaultConstructor {
			constructorName = "Inode",
			constructorFields = [
				(defaultField :: Field (Application Concrete Concrete)) {
					fieldName = "owner",
					fieldSemantics = "File owner.",
					fieldType = Variable First
				},
				(defaultField :: Field (Application Concrete Concrete)) {
					fieldName = "group",
					fieldSemantics = "File owner group.",
					fieldType = Variable First
				},
				(defaultField :: Field (Application Concrete Concrete)) {
					fieldName = "setuid",
					fieldSemantics = "Set user ID flag.",
					fieldType = DataType "0219c59f732a8ef507215fbdb4cceacd"
				},
				(defaultField  :: Field (Application Concrete Concrete)){
					fieldName = "setgid",
					fieldSemantics = "Set group ID flag.",
					fieldType = DataType "0219c59f732a8ef507215fbdb4cceacd"
				},
				(defaultField :: Field (Application Concrete Concrete)) {
					fieldName = "sticky",
					fieldSemantics = "Sticky bit.",
					fieldType = DataType "0219c59f732a8ef507215fbdb4cceacd"
				},
				(defaultField :: Field (Application Concrete Concrete)) {
					fieldName = "uperm",
					fieldSemantics = "User access permission mask.",
					fieldType = DataType "90eceef9-1189-4a18-903b-9cf36eb18e97"
				},
				(defaultField :: Field (Application Concrete Concrete)) {
					fieldName = "gperm",
					fieldSemantics = "Group access permission mask.",
					fieldType = DataType "90eceef9-1189-4a18-903b-9cf36eb18e97"
				},
				(defaultField :: Field (Application Concrete Concrete)) {
					fieldName = "operm",
					fieldSemantics = "Other access permission mask.",
					fieldType = DataType "90eceef9-1189-4a18-903b-9cf36eb18e97"
				},
				(defaultField :: Field (Application Concrete Concrete)) {
					fieldName = "mask",
					fieldSemantics = "Maximum permissions to be granted via access control lists.",
					fieldType = DataType "90eceef9-1189-4a18-903b-9cf36eb18e97"
				},
				(defaultField :: Field (Application Concrete Concrete)) {
					fieldName = "usracl",
					fieldSemantics = "User access control list.",
					fieldType =
						(Application
							(Application
								(DataType "43c6cd13-33b0-4fc8-a480-668ecb24768e")
								(Variable First)
							)
							(DataType "90eceef9-1189-4a18-903b-9cf36eb18e97")
						)
				},
				(defaultField :: Field (Application Concrete Concrete)) {
					fieldName = "grpacl",
					fieldSemantics = "Group access control list.",
					fieldType =
						(Application
							(Application
								(DataType "43c6cd13-33b0-4fc8-a480-668ecb24768e")
								(Variable First)
							)
							(DataType "90eceef9-1189-4a18-903b-9cf36eb18e97")
						)
				},
				(defaultField :: Field (Application Concrete Concrete)) {
					fieldName = "mtime",
					fieldSemantics = "Time of last modification.",
					fieldType = DataType "add67dbc-2e18-4ffd-aea3-b1e8cb28f7d8"
				},
				(defaultField :: Field (Application Concrete Concrete)) {
					fieldName = "ctime",
					fieldSemantics = "Time of file creation.",
					fieldType = DataType "add67dbc-2e18-4ffd-aea3-b1e8cb28f7d8"
				},
				(defaultField :: Field  (Application Concrete Concrete)) {
					fieldName = "xattr",
					fieldSemantics = "Extended attributes.",
					fieldType =
						(Application
							(Application
								(DataType "43c6cd13-33b0-4fc8-a480-668ecb24768e")
								(DataType "4f7db06c439541658a09689d3e7dd909")
							)
							(DataType "f9f2f27af0f649b4bc8946c467c3b76a")
						)
				},
				(defaultField :: Field (Application Concrete Concrete)) {
					fieldName = "file",
					fieldSemantics = "The actual file.",
					fieldType =
						(Application
							(DataType "027770dd-5134-4ee0-8cd8-faf29e962167")
							(Variable First)
						)
				}
			]
		}
	]
}

t65     :: TypeDefinition Concrete 
t65 = defaultType {
	identifier = "f2c4f6dd-d939-444b-a209-fbdf2152eb54",
  author       = Just personMikael,
	name = "SchemeSymbol",
	semantics = "URI scheme symbol.",
	constructors = Just [
		defaultConstructor {
			constructorName = "Latin",
			constructorFields = [
				defaultField {
					fieldName = "latin",
					fieldType = DataType "6716d098a58743379e54c12f249cdc0c"
				}
			]
		},
		defaultConstructor {
			constructorName = "Decimal",
			constructorFields = [
				defaultField {
					fieldName = "decimal",
					fieldType = DataType "ff421b2c31774c37a7336c8245a74da9"
				}
			]
		},
		defaultConstructor { constructorName = "Plus" },
		defaultConstructor { constructorName = "Minus" },
		defaultConstructor { constructorName = "FullStop" }
	]
}

t66     :: TypeDefinition Concrete 
t66 = defaultType {
	identifier = "6e2f1233-f1c8-4e6b-9bb3-7c405c666234",
  author       = Just personMikael,
	name = "SchemeName",
	semantics = "URI scheme.",
	constructors = Just [
		defaultConstructor {
			constructorName = "SchemeName",
			constructorFields = [
				defaultField {
					fieldName = "initial",
					fieldType = DataType "6716d098a58743379e54c12f249cdc0c"
				},
				defaultField {
					fieldName = "rest",
					fieldType =
						(Application
							(DataType "0ba85f3f10099c75d4b696d0cf944e09")
							(DataType "f2c4f6dd-d939-444b-a209-fbdf2152eb54")
						)
				}
			]
		}
	]
}

t67     :: TypeDefinition Concrete 
t67 = defaultType {
	identifier = "a078d512-3ead-415d-8d85-7dc6dc15b475",
  author       = Just personMikael,
	name = "RootlessPath",
	semantics = "Root-less path name.",
	constructors = Just [
		defaultConstructor {
			constructorName = "RootlessPath",
			constructorFields = [
				(defaultField  :: Field Concrete) {
					fieldName = "segments",
					fieldSemantics = "Path segments.",
					fieldType =
						(Application
							(DataType "0ba85f3f10099c75d4b696d0cf944e09")
							(DataType "4f7db06c439541658a09689d3e7dd909")
						)
				}
			]
		}
	]
}

t68     :: TypeDefinition Concrete 
t68 = defaultType {
	identifier = "6ffbfb86-82ad-4f6a-89d7-3e6d36c8fc7a",
  author       = Just personMikael,
	name = "AbsolutePath",
	semantics = "Absolute path name.",
	constructors = Just [
		defaultConstructor {
			constructorName = "AbsolutePath",
			constructorFields = [
				(defaultField :: Field Concrete) {
					fieldName = "segments",
					fieldSemantics = "Path segments.",
					fieldType =
						(Application
							(DataType "0ba85f3f10099c75d4b696d0cf944e09")
							(DataType "4f7db06c439541658a09689d3e7dd909")
						)
				}
			]
		}
	]
}

t69     :: TypeDefinition Concrete 
t69 = defaultType {
	identifier = "5448c6b7-9a08-4b4e-a40d-442c4fd2e125",
  author       = Just personMikael,
	name = "Path",
	semantics = "Path name.",
	constructors = Just [
		defaultConstructor {
			constructorName = "AbsolutePath",
			constructorFields = [
				defaultField {
					fieldName = "absolute",
					fieldType = DataType "6ffbfb86-82ad-4f6a-89d7-3e6d36c8fc7a"
				}
			]
		},
		defaultConstructor {
			constructorName = "RootlessPath",
			constructorFields = [
				defaultField {
					fieldName = "rootless",
					fieldType = DataType "a078d512-3ead-415d-8d85-7dc6dc15b475"
				}
			]
		}
	]
}

t70     :: TypeDefinition Concrete 
t70 = defaultType {
	identifier = "8068cbda-f35e-4618-a7e7-98c67ff9bee0",
  author       = Just personMikael,
	name = "Hierarchy",
	semantics = "URI hierarchy",
	constructors = Just [
		defaultConstructor {
			constructorName = "Authority",
			constructorFields = [
				defaultField {
					fieldName = "authority",
					fieldType = DataType "335b7633-0e72-4b64-a525-6190fb579dad"
				},
				defaultField {
					fieldName = "absolute",
					fieldType = DataType "6ffbfb86-82ad-4f6a-89d7-3e6d36c8fc7a"
				}
			]
		},
		defaultConstructor {
			constructorName = "Path",
			constructorFields = [
				defaultField {
					fieldName = "rootless",
					fieldType = DataType "a078d512-3ead-415d-8d85-7dc6dc15b475"
				}
			]
		}
	]
}

t71     :: TypeDefinition Concrete 
t71 = defaultType {
	identifier = "e393b15b-944c-4b35-97cd-02b1be6d693b",
  author       = Just personMikael,
	name = "URI",
	semantics = "Uniform Resource Identifier",
	constructors = Just [
		defaultConstructor {
			constructorName = "URI",
			constructorFields = [
				defaultField {
					fieldName = "scheme",
					fieldType = DataType "6e2f1233-f1c8-4e6b-9bb3-7c405c666234"
				},
				defaultField {
					fieldName = "hierarchy",
					fieldType = DataType "8068cbda-f35e-4618-a7e7-98c67ff9bee0"
				},
				defaultField {
					fieldName = "query",
					fieldType = DataType "4f7db06c439541658a09689d3e7dd909"
				},
				defaultField {
					fieldName = "fragment",
					fieldType = DataType "4f7db06c439541658a09689d3e7dd909"
				}
			]
		}
	]
}

t74     :: TypeDefinition Concrete 
t74 = defaultType {
	identifier = "a9c05900-6c8d-4849-af90-2d3ad12ee3cc",
  author       = Just personMikael,
	name = "IP",
	semantics = "Internet Protocol address.",
	constructors = Just [
		(defaultConstructor :: Constructor Concrete) {
			constructorName = "IPv6",
			constructorSemantics = "IP version 6 address.",
			constructorFields = [
				defaultField {
					fieldName = "ipv6",
					fieldType = DataType "bbabbac1510d49aa9da25d8033147c54"
				}
			]
		},
		(defaultConstructor :: Constructor Concrete) {
			constructorName = "IPv4",
			constructorSemantics = "IP version 4 address.",
			constructorFields = [
				defaultField {
					fieldName = "ipv4",
					fieldType = DataType "1a55145e5bd21e8adc14067707192552"
				}
			]
		}
	]
}

t75     :: TypeDefinition Concrete 
t75 = defaultType {
	identifier = "335b7633-0e72-4b64-a525-6190fb579dad",
  author       = Just personMikael,
	name = "Authority",
	constructors = Just [
		defaultConstructor {
			constructorName = "Authority",
			constructorFields = [
				(defaultField :: Field Concrete) {
					fieldName = "userinfo",
					fieldSemantics = "User information.",
					fieldType = DataType "4f7db06c439541658a09689d3e7dd909"
				},
				(defaultField :: Field Concrete) {
					fieldName = "host",
					fieldSemantics = "Host",
					fieldType = DataType "9f64aa56-7f1d-4456-b7ce-f6bf7f299c06"
				},
				(defaultField :: Field Concrete) {
					fieldName = "port",
					fieldSemantics = "Port number",
					fieldType =
						(Application
							(DataType "f8f49ef6bbe874a42926fa23d5b3bc19")
							(DataType "62d2d5371f08461aa328bc06561594f6")
						)
				}
			]
		}
	]
}

t76     :: TypeDefinition Concrete 
t76 = defaultType {
	identifier = "9f64aa56-7f1d-4456-b7ce-f6bf7f299c06",
  author       = Just personMikael,
	name = "Host",
	semantics = "URI host",
	constructors = Just [
		defaultConstructor {
			constructorName = "IP",
			constructorFields = [
				(defaultField :: Field Concrete) {
					fieldName = "ip",
					fieldSemantics = "IP address",
					fieldType = DataType "a9c05900-6c8d-4849-af90-2d3ad12ee3cc"
				}
			]
		},
		defaultConstructor {
			constructorName = "RegName",
			constructorFields = [
				(defaultField :: Field Concrete) {
					fieldName = "regName",
					fieldSemantics = "Registered name",
					fieldType = DataType "4f7db06c439541658a09689d3e7dd909"
				}
			]
		}
	]
}
