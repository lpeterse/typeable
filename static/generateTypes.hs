{-# OPTIONS -XNoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Main where

import Typeable.T9e2e1e478e094a8abe5507f8574ac91f --Succ
import Typeable.T421496848904471ea3197f25e2a02b72 --Zero
import Typeable.T606f253533d3420da3465afae341d598 --Time
import Typeable.T1660b01f08dc4aedbe4c0941584541cb --Kind
import Typeable.T0174bd2264004820bfe34e211cb35a7d hiding (constraints)--DataType
import qualified Typeable.T205895c8d2df475b8d5ead5ee33d9f63 as Field --Field
import qualified Typeable.T37c8a341f0b34cc6bbbc9f2403f09be3 as Constructor
import Typeable.T3e81531118e14888be21de7921b15bb5 -- Type
import Typeable.T451f847e1cb642d0b7c5dbdfa03f41b5 --Definition

import Prelude hiding (maybe)

import Typeable.Internal.TypesDefault
import Typeable.Internal.InternalTypeDefs
import qualified Data.Set as S
import qualified Data.Map as M
import Data.Ratio

import Data.EBF
import qualified Data.ByteString.Lazy as LBS

main :: IO ()
main  = mapM_ f types
        where
          f x = do LBS.writeFile ("types/T"++(show' $ identifier x)++".ebf") $ writeV00 x
                   putStrLn $ "Generate "++(show' $ identifier x)++"."++(show' $ name x)++"... Done."

dt :: Definition Type
dt  = Definition 
        { identifier       = undefined
        , antecedent       = Nothing
        , creationTime     = Time 3499718400
        , modificationTime = Time 3499718400
        , author           = Nothing
        , maintainer       = defaultPerson
        , name             = undefined
        , structure        = undefined  
        }

dt' = Type { semantics = "", constructors = Nothing }

defaultConstructor :: forall a. PeanoNumber a => Constructor.Constructor a
defaultConstructor  = Constructor.Constructor {
                        Constructor.name       = undefined
                      , Constructor.semantics  = ""  
                      , Constructor.fields     = []  :: [Field.Field a]
                      }

defaultField       :: (PeanoNumber a) => Field.Field a
defaultField        = Field.Field {
                        Field.name             = undefined 
                      , Field.semantics        = ""       
                      , Field.type_            = undefined -- :: Type a
                      }

defaultConstructor' :: Constructor.Constructor (Succ Zero)
defaultConstructor'  = defaultConstructor
defaultConstructor'' :: Constructor.Constructor (Succ (Succ Zero))
defaultConstructor''  = defaultConstructor
defaultConstructor''' :: Constructor.Constructor (Succ (Succ (Succ Zero)))
defaultConstructor'''  = defaultConstructor

defaultField' :: Field.Field (Succ Zero)
defaultField'  = defaultField
defaultField'' :: Field.Field (Succ (Succ Zero))
defaultField''  = defaultField
defaultField''' :: Field.Field (Succ (Succ (Succ Zero)))
defaultField'''  = defaultField

types  :: [Definition Type]
types   = [  t1
           , t2
           , t3
           , t4
           , t5
           , t6
           , t7
           , t8
           , t9
           , t10
           , t11
           , t12
           , t13
           , t14
           , t15
           , t16
           , t17
           , t18
           , t19
           , t20
           , t21
           , t22
           , t23
           , t24
           , t25
           , t26
           , t27
           , t28
           , t29
           , t31
           , t32
           , t33
           , t34
           , t35
           , t36
           , t37
           , t38
           , t39
           , t40
           , t41
           , t42
           , t43
           , t44
           , t45
           , t46
           , t47
           , t48
           , t49
           , t50
           , t51
           , t52
           , t53
           , t54
           , t55
           , t56
           , t57
           , t65
           , t66
           , t67
           , t68
           , t69
           , t70
           , t71
           , t74
           , t75
           , t76
           , t80
           , t81
           , t82
           , t83
           , t84
           , t85
           , t86
           , t87
           , t90
           , t93 -- Ordering
           , t94 -- Extension
           , t95 -- Extension
           , t97 -- Kind
           , t98 -- Pattern
           , t99 -- SimpleDialog
           , t100 -- SimpleMeta
           , t101 -- Turn
           , t102 -- SimpleSpeaker
           , t103 -- SimpleSpeaker
        ]

-----
-- type-definitions (provisoric for as long as the binary format is not yet finalised)
-----

t1       = dt {
             identifier   = "0219c59f732a8ef507215fbdb4cceacd"
           , name         = "Bool"
           , structure    = v0 $ (dt' :: Type Zero)
                            {   semantics    = "Boolean truth value." 
                            , constructors = Just [
                                     defaultConstructor { Constructor.name = "False" }
                                   , defaultConstructor { Constructor.name = "True"  }
                                   ]
                            }
            }

t56      = dt {
             identifier   = "42149684-8904-471e-a319-7f25e2a02b72"
           , author       = Just personLars
           , name         = "Zero"
           , structure    = v0 $ (dt' :: Type Zero)
             { semantics    = "The typelevel number zero. Interpreted as a set it is the empty set. It therefore has no instances."
             , constructors = Just []
             }
           }

t57      = dt {
             identifier   = "9e2e1e47-8e09-4a8a-be55-07f8574ac91f"
           , author       = Just personLars
           , name         = "Succ"
           , structure    = v1 $ (dt' :: Type (Succ Zero))
           { semantics    = "Counting in the peano sense. Interpreted as a set it is the set that contains n ordinal numbers."
           , constructors = Just [
                                   defaultConstructor { Constructor.name   = "First" }
                                 , defaultConstructor { Constructor.name   = "Next"
                                                       , Constructor.fields = [ defaultField
                                                                               { Field.name = "previous"
                                                                               , Field.type_ = Variable First
                                                                               }
                                                                             ]
                                                       }
                                 ]
           }
           }

t103      = dt {
             identifier   = "964f0aba-d1bd-422f-aaf5-7b6d9938db31"
           , author       = Nothing
           , name         = "Tree"
           , structure    = v1 $ (dt' :: Type (Succ Zero))
           { semantics    = "Multi-way trees, also known as rose trees."
           , constructors = Just [
                                    defaultConstructor { Constructor.name   = "Tree"
                                                       , Constructor.fields = [ defaultField
                                                                               { Field.name = "rootLabel"
                                                                               , Field.type_ = Variable First
                                                                               }
                                                                              , defaultField
                                                                               { Field.name = "subForest"
                                                                               , Field.type_ = list $ Application (DataType "964f0aba-d1bd-422f-aaf5-7b6d9938db31") (Variable First)
                                                                               }
                                                                             ]
                                                       }
                                 ]
           }
           }

t93      = dt {
             identifier   = "f4b6d72c609d4003ba98917f8c56a678"
           , author       = Nothing
           , name         = "Ordering"
           , structure = v0 $ (dt' :: Type Zero)
             { semantics    = ""
             , constructors = Just [
                                     defaultConstructor { Constructor.name   = "LT", Constructor.semantics = "Less"}
                                   , defaultConstructor { Constructor.name   = "EQ", Constructor.semantics = "Equal"}
                                   , defaultConstructor { Constructor.name   = "GT", Constructor.semantics = "Greater"}
                                   ]
             }
           }

t98      = dt {
             identifier   = "9231d77f-8da7-460e-976d-7c5e4ff9b31b"
           , author       = Nothing
           , name         = "Pattern"
           , structure = v0 $ (dt' :: Type Zero)
             { semantics   = ""
             , constructors = Just [
                                     defaultConstructor { Constructor.name   = "MatchAll", Constructor.semantics = "Matches arbitraray expressions."}
                                   , defaultConstructor { Constructor.name   = "Constructor", Constructor.semantics = "Greater"}
                                   ]
             }
           }


t94      = dt {
             identifier   = "2c62454c586f4bdea5e2b17e432db245"
           , author       = Just personLars
           , name         = "Extension"
           , structure    = v1 $ (dt' :: Type (Succ Zero))
           { semantics    = "An extension for making it possible to reference types, classes and functions within structured text."
           , constructors = Just [
                                   defaultConstructor 
                                   { Constructor.name   = "Type"
                                   , Constructor.fields = [
                                                            defaultField 
                                                            { Field.name  = "type"
                                                            , Field.type_ = Application
                                                                              (DataType "0174bd2264004820bfe34e211cb35a7d")
                                                                              (Variable First)
                                                            }
                                                          ]
                                   }
                                 , defaultConstructor 
                                   { Constructor.name   = "ValueConstructor"      
                                   , Constructor.fields = [
                                                            defaultField 
                                                            { Field.name  = "reference"
                                                            , Field.type_ = DataType "346674042a7248b4a94abff0726d0c43"
                                                            }
                                                          , defaultField 
                                                            { Field.name  = "constructorIndex"
                                                            , Field.type_ = DataType "62d2d5371f08461aa328bc06561594f6"
                                                            }
                                                          ]
                                   }
                                 , defaultConstructor 
                                   { Constructor.name   = "ValueConstructorField"      
                                   , Constructor.fields = [
                                                            defaultField 
                                                            { Field.name  = "reference"
                                                            , Field.type_ = DataType "346674042a7248b4a94abff0726d0c43"
                                                            }
                                                          , defaultField 
                                                            { Field.name  = "constructorIndex"
                                                            , Field.type_ = DataType "62d2d5371f08461aa328bc06561594f6"
                                                            }
                                                          , defaultField 
                                                            { Field.name  = "fieldIndex"
                                                            , Field.type_ = DataType "62d2d5371f08461aa328bc06561594f6"
                                                            }
                                                          ]
                                   }
                                 , defaultConstructor { Constructor.name   = "Class"                }
                                 , defaultConstructor { Constructor.name   = "ClassMethod"          }
                                 , defaultConstructor { Constructor.name   = "Constraint"           }
                                 , defaultConstructor { Constructor.name   = "Expression"           }
                                 ]
           }}



t86      = dt {
             identifier   = "50eae3e8-5d2d-42c8-8754-b026cc360981"
           , author       = Nothing
           , name         = "Function"
           , structure    = v2 $ (dt' :: Type (Succ (Succ Zero)))  
           { semantics    = "A function: a total mapping from the domain $0 to the domain $1."
           , constructors = Nothing
           }
          }

t87      = dt {
             identifier   = "e590e9ce-9cea-4dfe-86a4-13e9270dd1c2"
           , author       = Nothing
           , name         = "Method"
           , structure    = v1 $ (dt' :: Type (Succ Zero))
           { semantics    = "A class method's name, signature and semantic."
           , constructors = Just [
                                   defaultConstructor
                                   { Constructor.name   = "Method"
                                   , Constructor.fields = [
                                                           defaultField
                                                           { Field.name = "name"
                                                           , Field.type_ = DataType "9790ade9814a4aaca5eaa80c3e47685d"
                                                           }
                                                         , defaultField
                                                           { Field.name = "signature"
                                                           , Field.type_ = Application (DataType "0174bd2264004820bfe34e211cb35a7d") (Variable First)
                                                           }
                                                         , defaultField
                                                           { Field.name = "semantics"
                                                           , Field.type_ = Application
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
          }



t52      = dt {
             identifier   = "2dbb6df8-73ad-4e4b-aeb8-2172074ed042"
           , author       = Just personLars
           , name         = "Gender"
           , structure    = v0 $ (dt' :: Type Zero)
           { constructors = Just [
                                   defaultConstructor { Constructor.name = "Male"    }
                                 , defaultConstructor { Constructor.name = "Female"  }
                                 ]
           }
          }

t53      = dt {
             identifier   = "a0bbed72-1166-4a31-9e09-dc1c0f97bbd6"
           , author       = Just personLars
           , name         = "Casus"
           , structure    = v0 $ (dt' :: Type Zero)
           { constructors = Just [
                                   defaultConstructor { Constructor.name = "Nominativus" }
                                 , defaultConstructor { Constructor.name = "Genetivus"   }
                                 , defaultConstructor { Constructor.name = "Dativus"     }
                                 , defaultConstructor { Constructor.name = "Accusativus" }
                                 , defaultConstructor { Constructor.name = "Ablativus"   }
                                 , defaultConstructor { Constructor.name = "Vocativus"   }
                                 ]
          }}

t54      = dt {
             identifier   = "a384955f-99d4-401c-a54a-3f9c62b78d0a"
           , author       = Just personLars
           , name         = "Numerus"
           , structure    = v0 $ (dt' :: Type Zero)
           { constructors = Just [
                                   defaultConstructor { Constructor.name = "Singularis" }
                                 , defaultConstructor { Constructor.name = "Pluralis"   }
                                 ]
          }}
 
t55      = dt {
             identifier   = "ce462e9d-f114-4a16-8188-6cd2619b5d1a"
           , author       = Just personLars
           , name         = "Genus"
           , structure = v0 $ (dt' :: Type Zero)
           { constructors = Just [
                                   defaultConstructor { Constructor.name = "Masculinum" }
                                 , defaultConstructor { Constructor.name = "Femininum"  }
                                 , defaultConstructor { Constructor.name = "Neutrum"  }
                                 ]
           }
          }
          
t49      = dt {
             identifier   = "3819884685d34bf19b3469304e15983d"
           , author       = Just personLars
           , name         = "Person"
           , structure    = v0 $ (dt' :: Type Zero)
           { semantics    = "A record of information for identifying someone."
           , constructors = Just [
                                   defaultConstructor
                                   { Constructor.name    = "Person"
                                   , Constructor.fields  = [
                                                            defaultField
                                                            { Field.name     = "name"
                                                            , Field.type_     = DataType "4f7db06c439541658a09689d3e7dd909"
                                                            }
                                                          , defaultField
                                                            { Field.name     = "contacts"
                                                            , Field.type_     = set $ DataType "53e0d483-a641-4425-9dce-752799d64305"
                                                            }
                                                          ]
                                   }
                                 ]
          }}


t97      = dt {
             identifier   = "1660b01f08dc4aedbe4c0941584541cb"
           , author       = Just personLars
           , name         = "Kind"
           , structure    = v0 $ (dt' :: Type Zero)
           { semantics    = "Representation of a type's kind."
           , constructors = Just [
                                   defaultConstructor
                                   { Constructor.name    = "KindStar"
                                   , Constructor.fields  = []
                                   }
                                 , defaultConstructor
                                   { Constructor.name    = "KindApplication"
                                   , Constructor.fields  = [
                                                            defaultField
                                                            { Field.name = "function"
                                                            , Field.type_ = DataType "1660b01f08dc4aedbe4c0941584541cb"
                                                            }
                                                          , defaultField
                                                            { Field.name = "argument"
                                                            , Field.type_ = DataType "1660b01f08dc4aedbe4c0941584541cb"
                                                            }
                                                          ]
                                   }
                                 ]
          }}


t48      = dt {
             identifier   = "9592f9fa4fae437a9e8d0917c14ff068"
           , author       = Just personLars
           , name         = "TextElement"
           , structure    = v1 $ (dt' :: Type (Succ Zero))
           { semantics    = "Used by -> StructuredText."
           , constructors = Just [
                                   defaultConstructor'
                                   { Constructor.name    = "Plaintext"
                                   , Constructor.fields  = [
                                                            defaultField'
                                                            { Field.name      = "text"
                                                            , Field.type_      = DataType "4f7db06c439541658a09689d3e7dd909"
                                                            }
                                                          , defaultField'
                                                            { Field.name      = "bold"
                                                            , Field.semantics = "States whether the text is to be printed bold."
                                                            , Field.type_      = DataType "0219c59f732a8ef507215fbdb4cceacd"
                                                            }
                                                          , defaultField'
                                                            { Field.name      = "italic"
                                                            , Field.semantics = "States whether the text is to be printed italic."
                                                            , Field.type_      = DataType "0219c59f732a8ef507215fbdb4cceacd"
                                                            }
                                                          , defaultField'
                                                            { Field.name      = "monospace"
                                                            , Field.semantics = "States whether the text is to be printed with fixed length characters."
                                                            , Field.type_      = DataType "0219c59f732a8ef507215fbdb4cceacd"
                                                            }
                                                          , defaultField'
                                                            { Field.name      = "cancelled"
                                                            , Field.semantics = "States whether the text is stroked out."
                                                            , Field.type_      = DataType "0219c59f732a8ef507215fbdb4cceacd"
                                                            }
                                                         ]
                                   }
                                 , defaultConstructor 
                                   { Constructor.name   = "Extension"
                                   , Constructor.fields = [
                                                           defaultField
                                                           { Field.name = "ext"
                                                           , Field.type_ = Variable First
                                                           }
                                                         ]
                                   }
                                 ]
          }}
          
t45      = dt {
             identifier   = "c1b1f6c7-22c2-436f-ab31-80146520814e"
           , author       = Just personMikael
           , name         = "UTC"
           , structure    = v0 $ (dt' :: Type Zero)
           { semantics    = "The UTC time standard."
           , constructors = Just [] 
           }}
          
t46      = dt {
             identifier   = "aaaa6ecd-826d-4604-801c-4fa962cc1446"
           , author       = Just personMikael
           , name         = "TAI"
           , structure    = v0 $ (dt' :: Type Zero)
           { semantics    = "The TAI time standard."
           , constructors = Just [] 
           }}
          
t47      = dt {
             identifier   = "2a94a7a8d4e049759d8dd546e72293ff"
           , name         = "Constraint"
           , structure    = v1 $ (dt' :: Type (Succ Zero))
           { semantics    = ""
           , constructors = Just [
                                   defaultConstructor'  
                                     { Constructor.name      = "Constraint"
                                     , Constructor.fields    = [   defaultField'
                                                                  { Field.name      = "class"
                                                                  , Field.type_      = (DataType "346674042a7248b4a94abff0726d0c43")
                                                                  }
                                                               ,  defaultField'  
                                                                  { Field.name      = "tail"
                                                                  , Field.semantics = "the remaining list"
                                                                  , Field.type_      = Application 
                                                                                       (DataType "0174bd2264004820bfe34e211cb35a7d") 
                                                                                       (Variable First)  
                                                                  }
                                                              ]
                                     } 
                                 ]
           } } 
          
          
t2       = dt {
             identifier   = "0ba85f3f10099c75d4b696d0cf944e09"
           , name         = "List"
           , structure    = v1 $ (dt' :: Type (Succ Zero))
           { semantics    = "This is the default type for listing something. The order of elements matters and elements may occur more than once."
           , constructors = Just [
                                   defaultConstructor'
                                     { Constructor.name      = "Nil"
                                     , Constructor.semantics = "The empty list. Terminates recursion."
                                     } 
                                 , defaultConstructor'  
                                     { Constructor.name      = "Cons"
                                     , Constructor.semantics = "This constructor prepends an element to a remaining list."
                                     , Constructor.fields    = [   defaultField'
                                                                  { Field.name      = "head"
                                                                  , Field.semantics = "element"
                                                                  , Field.type_      = (Variable First)
                                                                  }
                                                               ,  defaultField'  
                                                                  { Field.name      = "tail"
                                                                  , Field.semantics = "the remaining list"
                                                                  , Field.type_      = Application 
                                                                                       (DataType "0ba85f3f10099c75d4b696d0cf944e09") 
                                                                                       (Variable First)  
                                                                  }
                                                              ]
                                     } 
                                 ]
            }           }

t43       = dt {
             identifier   = "b0221a43-509e-4edd-b062-101bfd794bc4"
           , author       = Just personLars
           , name         = "StructuredText"
           , structure    = v1 $ (dt' :: Type (Succ Zero))
           { semantics    = "A Markup format."
           , constructors = Just [ 
                                   defaultConstructor
                                   { Constructor.name   = "Paragraph"
                                   , Constructor.fields = [
                                                           defaultField
                                                           { Field.name   = "paragraph"
                                                           , Field.type_   = Application
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
                                   { Constructor.name   = "IndentList"
                                   , Constructor.fields = [
                                                           defaultField
                                                           { Field.name   = "indentList"
                                                           , Field.type_   = Application
                                                                            (DataType "0ba85f3f10099c75d4b696d0cf944e09")
                                                                            (Application
                                                                              (DataType "b0221a43509e4eddb062101bfd794bc4")
                                                                              (Variable First)
                                                                            )
                                                           }
                                                         ]
                                   }
                                 , defaultConstructor
                                   { Constructor.name   = "BulletList"
                                   , Constructor.fields = [
                                                           defaultField
                                                           { Field.name   = "bulletList"
                                                           , Field.type_   = Application
                                                                            (DataType "0ba85f3f10099c75d4b696d0cf944e09")
                                                                            (Application
                                                                              (DataType "b0221a43509e4eddb062101bfd794bc4")
                                                                              (Variable First)
                                                                            )
                                                           }
                                                         ]
                                   }
                                 , defaultConstructor
                                   { Constructor.name   = "IndexedList"
                                   , Constructor.fields = [
                                                           defaultField
                                                           { Field.name   = "indexedList"
                                                           , Field.type_   = Application
                                                                            (DataType "0ba85f3f10099c75d4b696d0cf944e09")
                                                                            (Application
                                                                              (DataType "b0221a43509e4eddb062101bfd794bc4")
                                                                              (Variable First)
                                                                            )
                                                           }
                                                         ]
                                   }
                                 , defaultConstructor
                                   { Constructor.name   = "TitledList"
                                   , Constructor.fields = [
                                                           defaultField
                                                           { Field.name   = "titledList"
                                                           , Field.type_   = Application
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
          }}

t44       = dt {
             identifier   = "37c8a341f0b34cc6bbbc9f2403f09be3"
           , author       = Just personLars
           , name         = "Constructor"
           , structure    = v1 $ (dt' :: Type (Succ Zero))
           { semantics    = "A value constructor."
           , constructors = Just [ defaultConstructor 
                                   { Constructor.name      = "Constructor"
                                   , Constructor.fields    = [
                                                                  defaultField' 
                                                                  { Field.name      = "name"
                                                                  , Field.semantics = ""
                                                                  , Field.type_      = DataType "9790ade9814a4aaca5eaa80c3e47685d"  
                                                                  }
                                                               ,  defaultField' 
                                                                  { Field.name      = "semantics"
                                                                  , Field.semantics = ""
                                                                  , Field.type_      = Application 
                                                                                       (DataType "b0221a43-509e-4edd-b062-101bfd794bc4")
                                                                                       (Application
                                                                                         (DataType "2c62454c586f4bdea5e2b17e432db245")
                                                                                         (Variable First)
                                                                                       )
                                                                  }
                                                               ,  defaultField'  
                                                                  { Field.name      = "fields"
                                                                  , Field.semantics = ""
                                                                  , Field.type_      = list (Application (DataType "205895c8-d2df-475b-8d5e-ad5ee33d9f63") (Variable First))
                                                                  }
                                                               ]
                                   }
                                 ]
          }}

t51      = dt {
             identifier   = "205895c8-d2df-475b-8d5e-ad5ee33d9f63"
           , author       = Just personLars
           , name         = "Field"
           , structure    = v1 $ (dt' :: Type (Succ Zero))
           { semantics    = ""
           , constructors = Just [ defaultConstructor 
                                   { Constructor.name      = "Field"
                                   , Constructor.fields    = [
                                                                  defaultField'  
                                                                  { Field.name      = "name"
                                                                  , Field.semantics = ""
                                                                  , Field.type_      = DataType "9790ade9814a4aaca5eaa80c3e47685d"  
                                                                  }
                                                               ,  defaultField' 
                                                                  { Field.name      = "semantics"
                                                                  , Field.semantics = ""
                                                                  , Field.type_      = Application 
                                                                                       (DataType "b0221a43-509e-4edd-b062-101bfd794bc4")
                                                                                       (Application
                                                                                         (DataType "2c62454c586f4bdea5e2b17e432db245")
                                                                                         (Variable First)
                                                                                       )
                                                                  }
                                                               ,  defaultField' 
                                                                  { Field.name      = "type"
                                                                  , Field.semantics = ""
                                                                  , Field.type_      = Application 
                                                                                       (DataType "0174bd2264004820bfe34e211cb35a7d") 
                                                                                       (Variable First)
                                                                  }
                                                               ]
                                   }
                                 ]
          }}

t95      = dt {
             identifier   = "451f847e1cb642d0b7c5dbdfa03f41b5"
           , author       = Just personLars
           , name         = "Definition"
           , structure    = v1' (KindApplication KindStar KindStar)  $ (dt' :: Type (Succ Zero))
           { constructors = Just [
                                   defaultConstructor 
                                     { Constructor.name      = "Definition"
                                     , Constructor.fields    = [   defaultField'  
                                                                  { Field.name      = "identifier"
                                                                  , Field.semantics = "This identifier is bound to the structure and semantics, not to an actual version of discription etc."
                                                                  , Field.type_      = DataType "346674042a7248b4a94abff0726d0c43"
                                                                  }
                                                               ,  defaultField'  
                                                                  { Field.name      = "antecedent"
                                                                  , Field.semantics = "Note whether this is an improved version with changes to structure and/or semantics."
                                                                  , Field.type_      = Application 
                                                                                       (DataType "f8f49ef6bbe874a42926fa23d5b3bc19") 
                                                                                       (DataType "346674042a7248b4a94abff0726d0c43")  
                                                                  }
                                                               ,  defaultField'  
                                                                  { Field.name      = "name"
                                                                  , Field.semantics = "A human readable designation. It doesn't need to be unique. In doubt choose a short one that already catches the semantics as good as possible."
                                                                  , Field.type_      = DataType "9790ade9814a4aaca5eaa80c3e47685d"  
                                                                  }
                                                               ,  defaultField'  
                                                                  { Field.name      = "creationTime"
                                                                  , Field.semantics = "The date of creating structure and semantics."
                                                                  , Field.type_      = Application 
                                                                                       (DataType "606f253533d3420da3465afae341d598")
                                                                                       (DataType "c1b1f6c7-22c2-436f-ab31-80146520814e")
                                                                  }
                                                               ,  defaultField'
                                                                  { Field.name      = "modificationTime"
                                                                  , Field.semantics = "The date of the last improvement/modification to the description."
                                                                  , Field.type_      = Application 
                                                                                       (DataType "606f253533d3420da3465afae341d598")
                                                                                       (DataType "c1b1f6c7-22c2-436f-ab31-80146520814e")
                                                                  }
                                                               ,  defaultField' 
                                                                  { Field.name      = "author"
                                                                  , Field.semantics = "The original author. Nothing denotes that it belongs to the public domain."
                                                                  , Field.type_      = maybe $ DataType "38198846-85d3-4bf1-9b34-69304e15983d"  
                                                                  }
                                                               ,  defaultField' 
                                                                  { Field.name      = "maintainer"
                                                                  , Field.semantics = "Who is responsible for changes/additions to the description etc.?"
                                                                  , Field.type_      =  DataType "38198846-85d3-4bf1-9b34-69304e15983d"   
                                                                  }
                                                               ,  defaultField'
                                                                  { Field.name      = "structure"
                                                                  , Field.semantics = ""
                                                                  , Field.type_      = Application
                                                                                       (Variable First)
                                                                                       (DataType "421496848904471ea3197f25e2a02b72")     --Zero
                                                                  }
                                                           ]
                                     } 
                   ]
            }   }


t42      = dt {
             identifier   = "3e815311-18e1-4888-be21-de7921b15bb5"
           , author       = Just personLars
           , name         = "Type"
           , structure    = v1 $ (dt' :: Type (Succ Zero))
           { semantics    = "This is the datatype the whole system relies on :-)"
           , constructors = Just [
                                   defaultConstructor' 
                                     { Constructor.name      = "Type"
                                     , Constructor.fields    = [
                                                                  defaultField' 
                                                                  { Field.name      = "semantics"
                                                                  , Field.semantics = "The type's semantics in general. Details may be described in the constructors or fields."
                                                                  , Field.type_      = Application 
                                                                                      (DataType "b0221a43-509e-4edd-b062-101bfd794bc4")
                                                                                      (Application
                                                                                        (DataType "2c62454c586f4bdea5e2b17e432db245")
                                                                                        (Variable First)
                                                                                      )
                                                                  }
                                                               ,  defaultField'
                                                                  { Field.name      = "constructors"
                                                                  , Field.semantics = "The type's value constructors. If it is nothing, this means the type is abstract. Otherwise the constructors are listed whereas the order is relevant. Note the semantic difference between just the empty list and nothing."
                                                                  , Field.type_      = Application 
                                                                                       (DataType "f8f49ef6bbe874a42926fa23d5b3bc19")  
                                                                                       (list $ Application (DataType "37c8a341-f0b3-4cc6-bbbc-9f2403f09be3") (Variable First))
                                                                  }
                                                           ]
                                     } 
 
                                 , defaultConstructor' 
                                     { Constructor.name      = "Quantification"
                                     , Constructor.fields    = [
                                                                  defaultField' 
                                                                  { Field.name      = "kind"
                                                                  , Field.type_      = DataType "1660b01f08dc4aedbe4c0941584541cb" 
                                                                  }
                                                               ,  defaultField'
                                                                  { Field.name      = "quantified"
                                                                  , Field.type_      = Application 
                                                                                       (DataType "3e815311-18e1-4888-be21-de7921b15bb5")  
                                                                                       (Application (DataType "9e2e1e478e094a8abe5507f8574ac91f") (Variable First))
                                                                  }
                                                           ]
                                     } 
                   ]
            }           }

t80      = dt {
             identifier   = "4e0b8f8e-a2b1-4522-8fa4-ec74b559bf6a"
           , author       = Just personLars
           , name         = "Class"
           , structure    = v1 $ (dt' :: Type (Succ Zero))
           { constructors = Just [
                                   defaultConstructor 
                                     { Constructor.name      = "Class"
                                     , Constructor.fields    = [   defaultField'
                                                                  { Field.name      = "semantics"
                                                                  , Field.semantics = "The classes' semantics in general."
                                                                  , Field.type_      = Application 
                                                                                      (DataType "b0221a43-509e-4edd-b062-101bfd794bc4")
                                                                                      (Application
                                                                                        (DataType "2c62454c586f4bdea5e2b17e432db245")
                                                                                        (Variable First)
                                                                                      )
                                                                  }
                                                               ,  defaultField' 
                                                                  { Field.name      = "constraints"
                                                                  , Field.semantics = "Constraints on the type's free variables."
                                                                  , Field.type_      = Application
                                                                                       (DataType "7af30cce93724981a16a80f3f193dc33")
                                                                                       (Application
                                                                                         (DataType "2a94a7a8-d4e0-4975-9d8d-d546e72293ff")
                                                                                         (Variable First)
                                                                                       )
                                                                  }
                                                              ,   defaultField' 
                                                                  { Field.name      = "methods"
                                                                  , Field.semantics = ""
                                                                  , Field.type_      = list (Application
                                                                                         (DataType "e590e9ce9cea4dfe86a413e9270dd1c2")
                                                                                         (Variable First)
                                                                                       )
                                                                  }
                                                           ]
                                    }
                                  , defaultConstructor 
                                     { Constructor.name      = "Quantification"
                                     , Constructor.fields    = [
                                                                  defaultField' 
                                                                  { Field.name      = "kind"
                                                                  , Field.type_      = DataType "1660b01f08dc4aedbe4c0941584541cb" 
                                                                  }
                                                               ,  defaultField'
                                                                  { Field.name      = "quantified"
                                                                  , Field.type_      = Application 
                                                                                       (DataType "4e0b8f8ea2b145228fa4ec74b559bf6a")  
                                                                                       (Application (DataType "9e2e1e478e094a8abe5507f8574ac91f") (Variable First))
                                                                  }
                                                           ]
                                     } 
                                 ]
            }           }


t85       = dt {
              identifier   = "53e0d483a64144259dce752799d64305"
            , author       = Just personLars
            , name         = "ContactInformation"
            , structure    = v0 $ (dt' :: Type Zero)
            { constructors = Just [
                                    (defaultConstructor :: Constructor.Constructor Zero)
                                    { Constructor.name      = "Email"
                                    , Constructor.semantics = ""
                                    , Constructor.fields    = [
                                                               (defaultField :: Field.Field Zero)
                                                               { Field.name      = "email"
                                                               , Field.type_      = Application
                                                                                    (DataType "1ea5eae4-7028-44f7-acbc-3c65b2a40093")
                                                                                    (DataType "ed098cc9-75df-4cd0-adb9-9a5b7dc48600")
                                                               }                                                               
                                                             ]
                                    }
                                  , (defaultConstructor  :: Constructor.Constructor Zero)
                                    { Constructor.name      = "Phone"
                                    , Constructor.semantics = ""
                                    , Constructor.fields    = [
                                                               (defaultField :: Field.Field Zero)
                                                               { Field.name      = "phone"
                                                               , Field.type_      = Application
                                                                                    (DataType "1ea5eae4-7028-44f7-acbc-3c65b2a40093")
                                                                                    (DataType "f18ae792-e532-4a68-a16f-11ea5c61442a")
                                                               }                                                               
                                                             ]
                                    }
                                  , (defaultConstructor :: Constructor.Constructor Zero)
                                    { Constructor.name      = "Website"
                                    , Constructor.semantics = ""
                                    , Constructor.fields    = [
                                                               defaultField
                                                               { Field.name      = "website"
                                                               , Field.type_      = Application
                                                                                    (DataType "1ea5eae4-7028-44f7-acbc-3c65b2a40093")
                                                                                    (DataType "d847a61a-1a94-4723-ab4b-fcfb214bd8aa")
                                                               }                                                               
                                                             ]
                                    }
                                  ]
            }}

t81       = dt {
              identifier   = "ed098cc9-75df-4cd0-adb9-9a5b7dc48600"
            , author       = Just personLars
            , name         = "Mailto"
            , structure    = v0 $ (dt' :: Type Zero)
            { semantics    = "The URI scheme 'mailto' according to RFC 2368."
            , constructors = Just []
            }}
 
t82       = dt {
              identifier   = "f18ae792-e532-4a68-a16f-11ea5c61442a"
            , author       = Just personLars
            , name         = "Tel"
            , structure    = v0 $ (dt' :: Type Zero)
            { semantics    = "The URI scheme 'tel' according to RFC 3966."
            , constructors = Just []
            }}
 
t83       = dt {
              identifier   = "d847a61a-1a94-4723-ab4b-fcfb214bd8aa"
            , author       = Just personLars
            , name         = "Http"
            , structure    = v0 $ (dt' :: Type Zero)
            { semantics    = "The URI scheme 'http' according to RFC 1738, RFC 2068, RFC 2616."
            , constructors = Just []
            }}
 
t84       = dt
            { identifier   = "1ea5eae4-7028-44f7-acbc-3c65b2a40093"
            , antecedent   = Just  "e393b15b-944c-4b35-97cd-02b1be6d693b"
            , author       = Just personLars
            , name         = "UriByScheme"
            , structure    = v1 $ (dt' :: Type (Succ Zero))
            { semantics    = "Uniform Resource Identifier variable in the scheme."
            , constructors = Just [
                                   defaultConstructor 
                                    { Constructor.name = "Uri"
                                    , Constructor.fields = [
                                                            defaultField 
                                                            { Field.name = "hierarchy"
                                                            , Field.type_ = DataType "8068cbda-f35e-4618-a7e7-98c67ff9bee0"
                                                            }
                                                          , 
                                                            defaultField 
                                                            { Field.name = "query"
				  				  				  				  				  				  , Field.type_ = DataType "4f7db06c439541658a09689d3e7dd909"
			    			    			    			    			    			    }
                                                          , 
                                                            defaultField
                                                            { Field.name = "fragment"
                                                            , Field.type_ = DataType "4f7db06c439541658a09689d3e7dd909"
                                                            }
                                                          ]
                                    }
                                  ]
				  	}} 			    			    			    			    			    
                                                            
t41       = dt {
             identifier   = "0174bd22-6400-4820-bfe3-4e211cb35a7d"
           , author       = Just personLars
           , name         = "DataType"
           , structure    =  v1 $ (dt' :: Type (Succ Zero))
           { semantics    = ""
           , constructors = Just [
                                   defaultConstructor'  
                                   { Constructor.name      = "DataType"
                                   , Constructor.semantics = "References another Datatype by -> UUID."
                                   , Constructor.fields    = [ defaultField 
                                                              { Field.name      = "reference"
                                                              , Field.type_      = DataType "346674042a7248b4a94abff0726d0c43"
                                                              }
                                                            ]
                                   } 
                                 , defaultConstructor'  
                                   { Constructor.name      = "Variable"
                                   , Constructor.semantics = "Bound variable. You supply the set of variables manually via $0"
                                   , Constructor.fields    = [ defaultField 
                                                              { Field.name      = "variable"
                                                              , Field.type_      = Variable First
                                                              }
                                                            ]
                                   } 
                                 , defaultConstructor' 
                                   { Constructor.name      = "Application"
                                   , Constructor.semantics = "Apply one type onto another yielding a type of lower kind."
                                   , Constructor.fields    = [ defaultField 
                                                              { Field.name      = "function"
                                                              , Field.type_      = Application 
                                                                                   (DataType "0174bd22-6400-4820-bfe3-4e211cb35a7d")
                                                                                   (Variable First)
                                                              }
                                                            , defaultField 
                                                              { Field.name      = "argument"
                                                              , Field.type_      = Application
                                                                                   (DataType "0174bd22-6400-4820-bfe3-4e211cb35a7d")
                                                                                   (Variable First)
                                                              }
                                                            ]
                                   } 
                                 , defaultConstructor' 
                                   { Constructor.name      = "Forall"
                                   , Constructor.semantics = "Introduces an additional variable and assciates it with constraints."
                                   , Constructor.fields    = [ defaultField 
                                                              { Field.name      = "constraints"
                                                              , Field.type_      = Application 
                                                                                   (DataType "7af30cce93724981a16a80f3f193dc33")
                                                                                   (Application 
                                                                                     (DataType "2a94a7a8d4e049759d8dd546e72293ff")
                                                                                     (Application
                                                                                       (DataType "9e2e1e478e094a8abe5507f8574ac91f")
                                                                                       (Variable First)
                                                                                     )
                                                                                   )
                                                              }
                                                            , defaultField 
                                                              { Field.name      = "expression"
                                                              , Field.type_      = Application
                                                                                   (DataType "0174bd22-6400-4820-bfe3-4e211cb35a7d")
                                                                                   (Application
                                                                                     (DataType "9e2e1e478e094a8abe5507f8574ac91f")
                                                                                     (Variable First)
                                                                                   )
                                                              }
                                                            ]
                                   } 
                                ]
            }           }
                       
t36      = dt {
             identifier   = "4f7db06c-4395-4165-8a09-689d3e7dd909"
           , name         = "Text"
           , structure    = v0 $ (dt' :: Type Zero)
           { semantics    = "Unicode text. This is the default type for text processing (NLP etc)."
           , constructors = Nothing
           }
          }

t37      = dt {
             identifier   = "f9f2f27a-f0f6-49b4-bc89-46c467c3b76a"
           , name         = "ByteString"
           , structure    = v0 $ (dt' :: Type Zero)
           { semantics    = "Sequence of Bytes (8bit). This is the default type for storing binary data. This type is not to be used for storing text! Use -> Text instead."
           , constructors = Nothing
           }
          }
                       
t38      = dt {
             identifier   = "c211e54d-6eef-4234-a7b6-75d5f696efe5"
           , name         = "Rational"
           , structure    = v0 $ (dt' :: Type Zero)
           { semantics    = "A rational number. May be implemented as a fraction of -> Integer and -> Wordeger."
           , constructors = Nothing
           }
          }

t39      = dt {
             identifier   = "5e5c664c-fc32-4271-b542-bf7ab0c9c104"
           , name         = "RationalUnsigned"
           , structure    = v0 $ (dt' :: Type Zero)
           { semantics    = "An unsigned rational number. May be implemented as a fraction of two -> Wordeger."
           , constructors = Nothing
           }
          }

t3       = dt {
             identifier   = "16f4245df3cc0b534f028235ff8aae16"
           , name         = "Char"
           , structure    = v0 $ (dt' :: Type Zero)
           { semantics    = "A point in the Unicode space."
           , constructors = Nothing
           }
          }

          
t4       = dt {
             identifier   = "c74c35ddb3ef689646c50be868d11bdf"
           , name         = "Float"
           , structure    = v0 $ (dt' :: Type Zero)
           { semantics    = "Single precision floating point number (IEEE 754)"
           , constructors = Nothing
           }
          }

                    
t5       = dt {
             identifier   = "4b19d19d959322ac0ccd319a4d275bd0"
           , name         = "Double"
           , structure    = v0 $ (dt' :: Type Zero)
           { semantics    = "Double precision floating point number (IEEE 754)"
           , constructors = Nothing
           }
          }


t6       = dt {
             identifier   = "ec78dc6268e4fe6fe6df461f40359d62"
           , name         = "Int8"
           , structure    = v0 $ (dt' :: Type Zero)
           { semantics    = "Signed integer types with well defined range. These types are to be used if it is important to emphasize the size limitations."
           , constructors = Nothing
          }}


t7       = dt {
             identifier   = "7ee200d207963cca2d2a49719e97e973"
           , name         = "Int16"
           , structure    = v0 $ (dt' :: Type Zero)
           { semantics    = "Signed integer types with well defined range. These types are to be used if it is important to emphasize the size limitations."
           , constructors = Nothing
          }}

t8       = dt {
             identifier   = "7b05ee3f0bbe6569f48d3947ec425493"
           , name         = "Int32"
           , structure    = v0 $ (dt' :: Type Zero)
           { semantics    = "Signed integer types with well defined range. These types are to be used if it is important to emphasize the size limitations."
           , constructors = Nothing
          }}

t9       = dt {
             identifier   = "cc620c86261c781e03c8efd9a974b1cf"
           , name         = "Int64"
           , structure    = v0 $ (dt' :: Type Zero)
           { semantics    = "Signed integer types with well defined range. These types are to be used if it is important to emphasize the size limitations."
           , constructors = Nothing
          }}

t10       = dt {
             identifier   = "7704e26b08886d6b8c3c788a3a0b2db0"
           , name         = "Word8"
           , structure    = v0 $ (dt' :: Type Zero)
           { semantics    = "unsigned integer types with well defined range. These types are to be used if it is important to emphasize the size limitations."
           , constructors = Nothing
          }}

t11      = dt {
             identifier   = "2b567f4ccc26027ea78edd227800fe94"
           , name         = "Word16"
           , structure    = v0 $ (dt' :: Type Zero)
           { semantics    = "unsigned integer types with well defined range. These types are to be used if it is important to emphasize the size limitations."
           , constructors = Nothing
           }
          }

t12       = dt {
             identifier   = "1a55145e5bd21e8adc14067707192552"
           , name         = "Word32"
           , structure    = v0 $ (dt' :: Type Zero)
           { semantics    = "unsigned integer types with well defined range. These types are to be used if it is important to emphasize the size limitations."
           , constructors = Nothing
          }}

t13       = dt {
             identifier   = "187e33b43715d8fe529de5014c864d85"
           , name         = "Word64"
           , structure    = v0 $ (dt' :: Type Zero)
           { semantics    = "unsigned integer types with well defined range. These types are to be used if it is important to emphasize the size limitations."
           , constructors = Nothing
          }}

t14       = dt {
             identifier   = "bbabbac1-510d-49aa-9da2-5d8033147c54"
           , name         = "Word128"
           , structure    = v0 $ (dt' :: Type Zero)
           { semantics    = "unsigned integer types with well defined range. These types are to be used if it is important to emphasize the size limitations."
           , constructors = Nothing
          }}

t15       = dt {
             identifier   = "90ce401b-d12d-4afc-be37-331bed9e0423"
           , name         = "Word256"
           , structure    = v0 $ (dt' :: Type Zero)
           { semantics    = "unsigned integer types with well defined range. These types are to be used if it is important to emphasize the size limitations."
           , constructors = Nothing
           }
          }

t16       = dt {
             identifier   = "c15eddf1-94ad-4428-8cba-be701d5ae517"
           , name         = "Word512"
           , structure    = v0 $ (dt' :: Type Zero)
           { semantics    = "unsigned integer types with well defined range. These types are to be used if it is important to emphasize the size limitations."
           , constructors = Nothing
          }}

t17       = dt {
             identifier   = "ac2e770f2132aced749ec197385ff552"
           , name         = "Int"
           , structure    = v0 $ (dt' :: Type Zero)
           { semantics    = "Signed integer. This type is to be used as the default signed integer type. Due to technical constraints the type is bounded. It is assumed that in most applications this bound doesn't get exceeded. If this happens to a problem use the arbitrary precision integer types."
           , constructors = Nothing
           }
          }

t18       = dt {
             identifier   = "8006b4b18388f841272dbebeee847723"
           , name         = "Integer"
           , structure    = v0 $ (dt' :: Type Zero)
           { semantics    = "Signed integer with arbitrary precision only limited by the machined resources. (Macht Integer bis der Speicher platzt!)."
           , constructors = Nothing
          }}

t19       = dt {
             identifier   = "62d2d537-1f08-461a-a328-bc06561594f6"
           , name         = "Word"
           , structure    = v0 $ (dt' :: Type Zero)
           { semantics    = "unsigned integer types with well defined range. These types are to be used if it is important to emphasize the size limitations."
           , constructors = Nothing
           }
          }

t20       = dt {
             identifier   = "982dce09-43f6-4a74-858f-f22c753ab01d"
           , name         = "Wordeger"
           , structure    = v0 $ (dt' :: Type Zero)
           { semantics    = "unsigned integer types with arbitrary precision only limited by the machined resources."
           , constructors = Nothing
           }
          }


t21       = dt {
             identifier   = "d9eef038b47d0820c160ceb8b6a89943"
           , name         = "Either"
           , structure    = v2 $ (dt' :: Type (Succ (Succ Zero)))
           { semantics    = "Denotes a choice between two possible values of different types. The Left value is typically used for something that informs about the reason of a failed computation"
           , constructors = Just [
                                   defaultConstructor''  
                                     { Constructor.name      = "Left"
                                     , Constructor.semantics = "This constructor denotes the unwanted result"
                                     , Constructor.fields    = [ defaultField'' 
                                                                  { Field.name      = "left"
                                                                  , Field.semantics = ""
                                                                  , Field.type_      = (Variable First)
                                                                  }
                                                              ]
                                     } 
                                 , defaultConstructor'' 
                                     { Constructor.name      = "Right"
                                     , Constructor.semantics = "This constructor denotes the wanted result"
                                     , Constructor.fields    = [ defaultField'' 
                                                                  { Field.name      = "right"
                                                                  , Field.semantics = ""
                                                                  , Field.type_      = (Variable (Next First))
                                                                  }
                                                              ]
                                     } 
                                 ]

          }}


t22      = dt {
             identifier   = "f8f49ef6bbe874a42926fa23d5b3bc19"
           , name         = "Maybe"
           , structure    = v1 $ (dt' :: Type (Succ Zero))
           { semantics    = "Expresses optionality."
           , constructors = Just [
                                   defaultConstructor'
                                     { Constructor.name      = "Nothing"
                                     , Constructor.semantics = "This constructor denotes the absence of a value"
                                     } 
                                 ,  defaultConstructor'
                                     { Constructor.name      = "Just"
                                     , Constructor.semantics = "This constructor wraps a value "
                                     , Constructor.fields    = [ defaultField'
                                                                  { Field.name      = "just"
                                                                  , Field.type_      = (Variable First)
                                                                  }
                                                              ]
                                     } 
                                 ]

          }}


t23       = dt {
             identifier   = "10f280df659654becb6e08122e846284"
           , name         = "Unit"
           , structure    = v0 $ (dt' :: Type Zero)
           { semantics    = "The empty tuple. () in Haskell syntax."
           , constructors = Just [defaultConstructor {Constructor.name = "Unit"}]
           }
          }

t24       = dt {
             identifier   = "34c13bdaac7d413ed735e64edcac7ff5"
           , name         = "Tuple"
           , structure    = v2 $ (dt' :: Type (Succ (Succ Zero)))
           { semantics    = "A tuple."
           , constructors = Just [
                                  defaultConstructor'' 
                                    { Constructor.name = "Tuple"
                                    , Constructor.fields = [ defaultField''
                                                              { Field.name = "fst"
                                                              , Field.type_ = (Variable First)
                                                              }
                                                              
                                                            , defaultField''
                                                              { Field.name = "snd"
                                                              ,  Field.type_ = (Variable (Next First))
                                                              }
                                                          ]
                                    
                                    }
                                  
                                 ]
          }}


t90       = dt {
             identifier   = "5cae969a-6574-48eb-bc2e-af7f31d7340f"
           , name         = "Triple"
           , structure    = v3 $ (dt' :: Type (Succ (Succ (Succ Zero))))
           { semantics    = "A triple."
           , constructors = Just [
                                  defaultConstructor 
                                    { Constructor.name = "Triple"
                                    , Constructor.fields = [ defaultField
                                                              { Field.name = "fst"
                                                              , Field.type_ = (Variable First)
                                                              }
                                                            , defaultField
                                                              { Field.name = "snd"
                                                              ,  Field.type_ = (Variable (Next First))
                                                              }
                                                            , defaultField
                                                              { Field.name = "thrd"
                                                              ,  Field.type_ = (Variable (Next (Next First)))
                                                              }
                                                          ]
                                    
                                    }
                                  
                                 ]
          }}

t25       = dt {
             identifier   = "ff421b2c-3177-4c37-a733-6c8245a74da9"
           , author       = Just personClemens
           , name         = "DecimalAlphabet"
           , structure    = v0 $ (dt' :: Type Zero)
           { semantics    = "The 10 ciphers of the decimal system"
           , constructors = Just [
                                   defaultConstructor { Constructor.name = "Zero"  }
                                 , defaultConstructor { Constructor.name = "One"   }
                                 , defaultConstructor { Constructor.name = "Two"   }
                                 , defaultConstructor { Constructor.name = "Three" }
                                 , defaultConstructor { Constructor.name = "Four"  }
                                 , defaultConstructor { Constructor.name = "Five"  }
                                 , defaultConstructor { Constructor.name = "Six"   }
                                 , defaultConstructor { Constructor.name = "Seven" }
                                 , defaultConstructor { Constructor.name = "Eight" }
                                 , defaultConstructor { Constructor.name = "Nine"  }
                                 
                                 ]
          }}


t26       = dt {
             identifier   = "45cc309e-ec2d-47f3-a7ed-3af50c84a392"
           , author       = Just personClemens
           , name         = "HexadecimalAlphabet"
           , structure    = v0 $ (dt' :: Type Zero)
           { semantics    = "The 10 ciphers of the hexadecimal system"
           , constructors = Just [
                                   defaultConstructor { Constructor.name = "Zero"  }
                                 , defaultConstructor { Constructor.name = "One"   }
                                 , defaultConstructor { Constructor.name = "Two"   }
                                 , defaultConstructor { Constructor.name = "Three" }
                                 , defaultConstructor { Constructor.name = "Four"  }
                                 , defaultConstructor { Constructor.name = "Five"  }
                                 , defaultConstructor { Constructor.name = "Six"   }
                                 , defaultConstructor { Constructor.name = "Seven" }
                                 , defaultConstructor { Constructor.name = "Eight" }
                                 , defaultConstructor { Constructor.name = "Nine"  }
                                 , defaultConstructor { Constructor.name = "Ten"  }
                                 , defaultConstructor { Constructor.name = "Eleven"  }
                                 , defaultConstructor { Constructor.name = "Twelve"  }
                                 , defaultConstructor { Constructor.name = "Thirteen"  }
                                 , defaultConstructor { Constructor.name = "Fourteen"  }
                                 , defaultConstructor { Constructor.name = "Fifteen"  }                                                                  
                                 
                                 ]
          }}
          
 
t27       = dt {
             identifier   = "6716d098-a587-4337-9e54-c12f249cdc0c"
           , author       = Just personClemens
           , name         = "LatinAlphabet"
           , structure    = v0 $ (dt' :: Type Zero)
           { semantics    = "The 26 letters of the latin alphabet"
           , constructors = Just [
                                   defaultConstructor { Constructor.name = "A"  }
                                 , defaultConstructor { Constructor.name = "B"   }
                                 , defaultConstructor { Constructor.name = "C"   }
                                 , defaultConstructor { Constructor.name = "D" }
                                 , defaultConstructor { Constructor.name = "E"  }
                                 , defaultConstructor { Constructor.name = "F"  }
                                 , defaultConstructor { Constructor.name = "G"   }
                                 , defaultConstructor { Constructor.name = "H" }
                                 , defaultConstructor { Constructor.name = "I" }
                                 , defaultConstructor { Constructor.name = "J"  }
                                 , defaultConstructor { Constructor.name = "K"  }
                                 , defaultConstructor { Constructor.name = "L"  }
                                 , defaultConstructor { Constructor.name = "M"  }
                                 , defaultConstructor { Constructor.name = "N"  }
                                 , defaultConstructor { Constructor.name = "O"  }
                                 , defaultConstructor { Constructor.name = "P"  }  
                                 , defaultConstructor { Constructor.name = "Q"  }
                                 , defaultConstructor { Constructor.name = "R"  }
                                 , defaultConstructor { Constructor.name = "S"  }
                                 , defaultConstructor { Constructor.name = "T"  }
                                 , defaultConstructor { Constructor.name = "U"  }
                                 , defaultConstructor { Constructor.name = "V"  }
                                 , defaultConstructor { Constructor.name = "W"  }
                                 , defaultConstructor { Constructor.name = "X"  }
                                 , defaultConstructor { Constructor.name = "Y"  }
                                 , defaultConstructor { Constructor.name = "Z"  }                                                                
                                 
                                 ]
          } }         
          

t28       = dt {
             identifier   = "1566edb1-a4de-4aab-8106-e63293e9bfcf"
           , author       = Just personClemens
           , name         = "Symbol"
           , structure    = v0 $ (dt' :: Type Zero)
           { semantics    = "the choice of symbols available in a -> Designator: Lower and Uppercase Latin Characters, decimal ciphers and the underscore"
           , constructors = Just [
                                   defaultConstructor { 
                                     Constructor.name = "Lower"
                                    ,Constructor.fields = [ defaultField {
                                                                   Field.name = "lower" 
                                                                 , Field.type_ = DataType "6716d098-a587-4337-9e54-c12f249cdc0c"  
                                                                 }
                                                         ]
                                   }
                                 , defaultConstructor { 
                                     Constructor.name = "Upper"
                                    ,Constructor.fields = [ defaultField {
                                                                   Field.name = "upper" 
                                                                 , Field.type_ = DataType "6716d098-a587-4337-9e54-c12f249cdc0c"  
                                                                 }
                                                         ]
                                   } 
                                   
                                 , defaultConstructor { 
                                     Constructor.name = "Decimal"
                                    ,Constructor.fields =  [ defaultField {
                                                                   Field.name = "decimal" 
                                                                ,  Field.type_ = DataType "ff421b2c-3177-4c37-a733-6c8245a74da9"
                                                                 }
                                                          ]
                                   }  
                                   
                                 , defaultConstructor { 
                                     Constructor.name = "Underscore"
                                    ,Constructor.fields = []
                                   }    
                                 , defaultConstructor { 
                                     Constructor.name = "Prime"
                                    ,Constructor.fields = []
                                   }
                                 ]
          }}
          
t29       = dt {
             identifier   = "9790ade9-814a-4aac-a5ea-a80c3e47685d"
           , author       = Just personClemens
           , name         = "Designator"
           , structure    = v0 $ (dt' :: Type Zero)
           { semantics    = "This type represents valid designators. These contain at least one -> latin character and a list of -> Symbols (lower/upper character, decimal character, underscore, prime) of arbitrary length. Also look how the context determines the semantics of the single LatinAlphabet character: It itself does not have any case but the context induces one."
           , constructors = Just [
                                   defaultConstructor { 
                                     Constructor.name = "Designator"
                                    ,Constructor.fields = [ defaultField {
                                                                   Field.name = "initial" 
                                                                ,  Field.type_ = DataType "6716d098-a587-4337-9e54-c12f249cdc0c"  
                                                                 }
                                                         , defaultField {
                                                                   Field.name = "subsequent" 
                                                                ,  Field.type_ = list $ DataType "1566edb1-a4de-4aab-8106-e63293e9bfcf"
                                                                 }
                                                         ]
                                   }           
                                 ]
          }}   
          
          
          
          
t31       = dt {
             identifier   = "7af30cce-9372-4981-a16a-80f3f193dc33"
           , name         = "Set"
           , structure    = v1 $ (dt' :: Type (Succ Zero))
           { semantics    = "A set. Elements are unique and unordered."
           , constructors = Nothing
           }
          }         
          
          

t32       = dt {
             identifier   = "43c6cd13-33b0-4fc8-a480-668ecb24768e"
           , name         = "Map"
           , structure    = v2 $ (dt' :: Type (Succ Zero))
           { semantics    = "Associates keys of type a with elements of type b."
           , constructors = Nothing
           }
          }               

t33       = dt {
             identifier   = "34667404-2a72-48b4-a94a-bff0726d0c43"
           , name         = "UUID"
           , structure    = v0 $ (dt' :: Type Zero)
           { semantics    = "A universally unique identifier. It is defined to be a natural number in range 0 to 2^128-1. For all practical applications this type is assumed to provide infinitely many values. The order induced by the definition is to be ignored."
           , constructors = Nothing
          }}

t40       = dt {
             identifier   = "606f2535-33d3-420d-a346-5afae341d598"
           , author       = Just personMikael
           , name         = "Time"
           , structure    = v1 $ (dt' :: Type (Succ Zero))
           { semantics    = "This type is used for noting a point in time. It is polymorphic in the timescale used. See http://en.wikipedia.org/wiki/Time_standard for details on this issue."
           , constructors = Just [
                                  defaultConstructor 
                                    { Constructor.name = "Time"
                                    , Constructor.fields = [ defaultField'
                                                              { Field.name      = "seconds"
                                                              , Field.type_      = DataType  "c211e54d6eef4234a7b675d5f696efe5"
                                                              , Field.semantics = "Seconds relative to January 1st, 1900. What is meant by a second depends on the timescale."
                                                              }
                                                          ]
                                    
                                    }
                                 ]
          }}


t50       = dt {
             identifier   = "b6831ec097f14b8eba74b1e486b4175d"
           , author       = Just personLars
           , name         = "Date"
           , structure    = v1 $ (dt' :: Type (Succ Zero))
           { semantics    = "This type is used for noting a day time. It is polymorphic in the timescale used. See http://en.wikipedia.org/wiki/Time_standard for details on this issue."
           , constructors = Just [
                                  defaultConstructor' 
                                    { Constructor.name = "Time"
                                    , Constructor.fields = [ defaultField'             
                                                              { Field.name      = "days"
                                                              , Field.type_      = DataType "ac2e770f2132aced749ec197385ff552"
                                                              , Field.semantics = "Days relative to January 1st, 1900. What is meant by a day depends on the timescale."
                                                              }
                                                          ]
                                    
                                    }
                                 ]
          }}



t34       = dt {
             identifier   = "f47867c1-1a4d-4e30-ab65-2240dd8e72ba"
           , name         = "Void"
           , structure    = v0 $ (dt' :: Type Zero)
           { semantics    = "This type has no constructors and therefore no instances. Don't mix it up with ->Unit which has exactly one instance."
           , constructors = Just []
          }}               

t35       = dt {
             identifier   = "af20e1db-8f0d-414f-9062-5b1521e41378"
           , name         = "Language"
           , structure    = v0 $ (dt' :: Type Zero)
           { semantics    = "Languages according to ISO 639-2T."
           , constructors = Just [
                                    defaultConstructor { Constructor.name = "AAR", Constructor.semantics = "Afar" }
                                  , defaultConstructor { Constructor.name = "ABK", Constructor.semantics = "Abkhazian" }
                                  , defaultConstructor { Constructor.name = "ACE", Constructor.semantics = "Achinese" }
                                  , defaultConstructor { Constructor.name = "ACH", Constructor.semantics = "Acoli" }
                                  , defaultConstructor { Constructor.name = "ADA", Constructor.semantics = "Adangme" }
                                  , defaultConstructor { Constructor.name = "ADY", Constructor.semantics = "Adyghe; Adygei" }
                                  , defaultConstructor { Constructor.name = "AFA", Constructor.semantics = "Afro-Asiatic languages" }
                                  , defaultConstructor { Constructor.name = "AFH", Constructor.semantics = "Afrihili" }
                                  , defaultConstructor { Constructor.name = "AFR", Constructor.semantics = "Afrikaans" }
                                  , defaultConstructor { Constructor.name = "AIN", Constructor.semantics = "Ainu" }
                                  , defaultConstructor { Constructor.name = "AKA", Constructor.semantics = "Akan" }
                                  , defaultConstructor { Constructor.name = "AKK", Constructor.semantics = "Akkadian" }
                                  , defaultConstructor { Constructor.name = "ALE", Constructor.semantics = "Aleut" }
                                  , defaultConstructor { Constructor.name = "ALG", Constructor.semantics = "Algonquian languages" }
                                  , defaultConstructor { Constructor.name = "ALT", Constructor.semantics = "Southern Altai" }
                                  , defaultConstructor { Constructor.name = "AMH", Constructor.semantics = "Amharic" }
                                  , defaultConstructor { Constructor.name = "ANG", Constructor.semantics = "English, Old (ca.450-1100)" }
                                  , defaultConstructor { Constructor.name = "ANP", Constructor.semantics = "Angika" }
                                  , defaultConstructor { Constructor.name = "APA", Constructor.semantics = "Apache languages" }
                                  , defaultConstructor { Constructor.name = "ARA", Constructor.semantics = "Arabic" }
                                  , defaultConstructor { Constructor.name = "ARC", Constructor.semantics = "Official Aramaic (700-300 BCE); Imperial Aramaic (700-300 BCE)" }
                                  , defaultConstructor { Constructor.name = "ARG", Constructor.semantics = "Aragonese" }
                                  , defaultConstructor { Constructor.name = "ARN", Constructor.semantics = "Mapudungun; Mapuche" }
                                  , defaultConstructor { Constructor.name = "ARP", Constructor.semantics = "Arapaho" }
                                  , defaultConstructor { Constructor.name = "ART", Constructor.semantics = "Artificial languages" }
                                  , defaultConstructor { Constructor.name = "ARW", Constructor.semantics = "Arawak" }
                                  , defaultConstructor { Constructor.name = "ASM", Constructor.semantics = "Assamese" }
                                  , defaultConstructor { Constructor.name = "AST", Constructor.semantics = "Asturian; Bable; Leonese; Asturleonese" }
                                  , defaultConstructor { Constructor.name = "ATH", Constructor.semantics = "Athapascan languages" }
                                  , defaultConstructor { Constructor.name = "AUS", Constructor.semantics = "Australian languages" }
                                  , defaultConstructor { Constructor.name = "AVA", Constructor.semantics = "Avaric" }
                                  , defaultConstructor { Constructor.name = "AVE", Constructor.semantics = "Avestan" }
                                  , defaultConstructor { Constructor.name = "AWA", Constructor.semantics = "Awadhi" }
                                  , defaultConstructor { Constructor.name = "AYM", Constructor.semantics = "Aymara" }
                                  , defaultConstructor { Constructor.name = "AZE", Constructor.semantics = "Azerbaijani" }
                                  , defaultConstructor { Constructor.name = "BAD", Constructor.semantics = "Banda languages" }
                                  , defaultConstructor { Constructor.name = "BAI", Constructor.semantics = "Bamileke languages" }
                                  , defaultConstructor { Constructor.name = "BAK", Constructor.semantics = "Bashkir" }
                                  , defaultConstructor { Constructor.name = "BAL", Constructor.semantics = "Baluchi" }
                                  , defaultConstructor { Constructor.name = "BAM", Constructor.semantics = "Bambara" }
                                  , defaultConstructor { Constructor.name = "BAN", Constructor.semantics = "Balinese" }
                                  , defaultConstructor { Constructor.name = "BAS", Constructor.semantics = "Basa" }
                                  , defaultConstructor { Constructor.name = "BAT", Constructor.semantics = "Baltic languages" }
                                  , defaultConstructor { Constructor.name = "BEJ", Constructor.semantics = "Beja; Bedawiyet" }
                                  , defaultConstructor { Constructor.name = "BEL", Constructor.semantics = "Belarusian" }
                                  , defaultConstructor { Constructor.name = "BEM", Constructor.semantics = "Bemba" }
                                  , defaultConstructor { Constructor.name = "BEN", Constructor.semantics = "Bengali" }
                                  , defaultConstructor { Constructor.name = "BER", Constructor.semantics = "Berber languages" }
                                  , defaultConstructor { Constructor.name = "BHO", Constructor.semantics = "Bhojpuri" }
                                  , defaultConstructor { Constructor.name = "BIH", Constructor.semantics = "Bihari languages" }
                                  , defaultConstructor { Constructor.name = "BIK", Constructor.semantics = "Bikol" }
                                  , defaultConstructor { Constructor.name = "BIN", Constructor.semantics = "Bini; Edo" }
                                  , defaultConstructor { Constructor.name = "BIS", Constructor.semantics = "Bislama" }
                                  , defaultConstructor { Constructor.name = "BLA", Constructor.semantics = "Siksika" }
                                  , defaultConstructor { Constructor.name = "BNT", Constructor.semantics = "Bantu languages" }
                                  , defaultConstructor { Constructor.name = "BOD", Constructor.semantics = "Tibetan" }
                                  , defaultConstructor { Constructor.name = "BOS", Constructor.semantics = "Bosnian" }
                                  , defaultConstructor { Constructor.name = "BRA", Constructor.semantics = "Braj" }
                                  , defaultConstructor { Constructor.name = "BRE", Constructor.semantics = "Breton" }
                                  , defaultConstructor { Constructor.name = "BTK", Constructor.semantics = "Batak languages" }
                                  , defaultConstructor { Constructor.name = "BUA", Constructor.semantics = "Buriat" }
                                  , defaultConstructor { Constructor.name = "BUG", Constructor.semantics = "Buginese" }
                                  , defaultConstructor { Constructor.name = "BUL", Constructor.semantics = "Bulgarian" }
                                  , defaultConstructor { Constructor.name = "BYN", Constructor.semantics = "Blin; Bilin" }
                                  , defaultConstructor { Constructor.name = "CAD", Constructor.semantics = "Caddo" }
                                  , defaultConstructor { Constructor.name = "CAI", Constructor.semantics = "Central American Indian languages" }
                                  , defaultConstructor { Constructor.name = "CAR", Constructor.semantics = "Galibi Carib" }
                                  , defaultConstructor { Constructor.name = "CAT", Constructor.semantics = "Catalan; Valencian" }
                                  , defaultConstructor { Constructor.name = "CAU", Constructor.semantics = "Caucasian languages" }
                                  , defaultConstructor { Constructor.name = "CEB", Constructor.semantics = "Cebuano" }
                                  , defaultConstructor { Constructor.name = "CEL", Constructor.semantics = "Celtic languages" }
                                  , defaultConstructor { Constructor.name = "CES", Constructor.semantics = "Czech" }
                                  , defaultConstructor { Constructor.name = "CHA", Constructor.semantics = "Chamorro" }
                                  , defaultConstructor { Constructor.name = "CHB", Constructor.semantics = "Chibcha" }
                                  , defaultConstructor { Constructor.name = "CHE", Constructor.semantics = "Chechen" }
                                  , defaultConstructor { Constructor.name = "CHG", Constructor.semantics = "Chagatai" }
                                  , defaultConstructor { Constructor.name = "CHK", Constructor.semantics = "Chuukese" }
                                  , defaultConstructor { Constructor.name = "CHM", Constructor.semantics = "Mari" }
                                  , defaultConstructor { Constructor.name = "CHN", Constructor.semantics = "Chinook jargon" }
                                  , defaultConstructor { Constructor.name = "CHO", Constructor.semantics = "Choctaw" }
                                  , defaultConstructor { Constructor.name = "CHP", Constructor.semantics = "Chipewyan; Dene Suline" }
                                  , defaultConstructor { Constructor.name = "CHR", Constructor.semantics = "Cherokee" }
                                  , defaultConstructor { Constructor.name = "CHU", Constructor.semantics = "Church Slavic; Old Slavonic; Church Slavonic; Old Bulgarian; Old Church Slavonic" }
                                  , defaultConstructor { Constructor.name = "CHV", Constructor.semantics = "Chuvash" }
                                  , defaultConstructor { Constructor.name = "CHY", Constructor.semantics = "Cheyenne" }
                                  , defaultConstructor { Constructor.name = "CMC", Constructor.semantics = "Chamic languages" }
                                  , defaultConstructor { Constructor.name = "COP", Constructor.semantics = "Coptic" }
                                  , defaultConstructor { Constructor.name = "COR", Constructor.semantics = "Cornish" }
                                  , defaultConstructor { Constructor.name = "COS", Constructor.semantics = "Corsican" }
                                  , defaultConstructor { Constructor.name = "CPE", Constructor.semantics = "Creoles and pidgins, English based" }
                                  , defaultConstructor { Constructor.name = "CPF", Constructor.semantics = "Creoles and pidgins, French-based" }
                                  , defaultConstructor { Constructor.name = "CPP", Constructor.semantics = "Creoles and pidgins, Portuguese-based" }
                                  , defaultConstructor { Constructor.name = "CRE", Constructor.semantics = "Cree" }
                                  , defaultConstructor { Constructor.name = "CRH", Constructor.semantics = "Crimean Tatar; Crimean Turkish" }
                                  , defaultConstructor { Constructor.name = "CRP", Constructor.semantics = "Creoles and pidgins" }
                                  , defaultConstructor { Constructor.name = "CSB", Constructor.semantics = "Kashubian" }
                                  , defaultConstructor { Constructor.name = "CUS", Constructor.semantics = "Cushitic languages" }
                                  , defaultConstructor { Constructor.name = "CYM", Constructor.semantics = "Welsh" }
                                  , defaultConstructor { Constructor.name = "DAK", Constructor.semantics = "Dakota" }
                                  , defaultConstructor { Constructor.name = "DAN", Constructor.semantics = "Danish" }
                                  , defaultConstructor { Constructor.name = "DAR", Constructor.semantics = "Dargwa" }
                                  , defaultConstructor { Constructor.name = "DAY", Constructor.semantics = "Land Dayak languages" }
                                  , defaultConstructor { Constructor.name = "DEL", Constructor.semantics = "Delaware" }
                                  , defaultConstructor { Constructor.name = "DEN", Constructor.semantics = "Slave (Athapascan)" }
                                  , defaultConstructor { Constructor.name = "DEU", Constructor.semantics = "German" }
                                  , defaultConstructor { Constructor.name = "DGR", Constructor.semantics = "Dogrib" }
                                  , defaultConstructor { Constructor.name = "DIN", Constructor.semantics = "Dinka" }
                                  , defaultConstructor { Constructor.name = "DIV", Constructor.semantics = "Divehi; Dhivehi; Maldivian" }
                                  , defaultConstructor { Constructor.name = "DOI", Constructor.semantics = "Dogri" }
                                  , defaultConstructor { Constructor.name = "DRA", Constructor.semantics = "Dravidian languages" }
                                  , defaultConstructor { Constructor.name = "DSB", Constructor.semantics = "Lower Sorbian" }
                                  , defaultConstructor { Constructor.name = "DUA", Constructor.semantics = "Duala" }
                                  , defaultConstructor { Constructor.name = "DUM", Constructor.semantics = "Dutch, Middle (ca.1050-1350)" }
                                  , defaultConstructor { Constructor.name = "DYU", Constructor.semantics = "Dyula" }
                                  , defaultConstructor { Constructor.name = "DZO", Constructor.semantics = "Dzongkha" }
                                  , defaultConstructor { Constructor.name = "EFI", Constructor.semantics = "Efik" }
                                  , defaultConstructor { Constructor.name = "EGY", Constructor.semantics = "Egyptian (Ancient)" }
                                  , defaultConstructor { Constructor.name = "EKA", Constructor.semantics = "Ekajuk" }
                                  , defaultConstructor { Constructor.name = "ELL", Constructor.semantics = "Greek, Modern (1453-)" }
                                  , defaultConstructor { Constructor.name = "ELX", Constructor.semantics = "Elamite" }
                                  , defaultConstructor { Constructor.name = "ENG", Constructor.semantics = "English" }
                                  , defaultConstructor { Constructor.name = "ENM", Constructor.semantics = "English, Middle (1100-1500)" }
                                  , defaultConstructor { Constructor.name = "EPO", Constructor.semantics = "Esperanto" }
                                  , defaultConstructor { Constructor.name = "EST", Constructor.semantics = "Estonian" }
                                  , defaultConstructor { Constructor.name = "EUS", Constructor.semantics = "Basque" }
                                  , defaultConstructor { Constructor.name = "EWE", Constructor.semantics = "Ewe" }
                                  , defaultConstructor { Constructor.name = "EWO", Constructor.semantics = "Ewondo" }
                                  , defaultConstructor { Constructor.name = "FAN", Constructor.semantics = "Fang" }
                                  , defaultConstructor { Constructor.name = "FAO", Constructor.semantics = "Faroese" }
                                  , defaultConstructor { Constructor.name = "FAS", Constructor.semantics = "Persian" }
                                  , defaultConstructor { Constructor.name = "FAT", Constructor.semantics = "Fanti" }
                                  , defaultConstructor { Constructor.name = "FIJ", Constructor.semantics = "Fijian" }
                                  , defaultConstructor { Constructor.name = "FIL", Constructor.semantics = "Filipino; Pilipino" }
                                  , defaultConstructor { Constructor.name = "FIN", Constructor.semantics = "Finnish" }
                                  , defaultConstructor { Constructor.name = "FIU", Constructor.semantics = "Finno-Ugrian languages" }
                                  , defaultConstructor { Constructor.name = "FON", Constructor.semantics = "Fon" }
                                  , defaultConstructor { Constructor.name = "FRA", Constructor.semantics = "French" }
                                  , defaultConstructor { Constructor.name = "FRM", Constructor.semantics = "French, Middle (ca.1400-1600)" }
                                  , defaultConstructor { Constructor.name = "FRO", Constructor.semantics = "French, Old (842-ca.1400)" }
                                  , defaultConstructor { Constructor.name = "FRR", Constructor.semantics = "Northern Frisian" }
                                  , defaultConstructor { Constructor.name = "FRS", Constructor.semantics = "Eastern Frisian" }
                                  , defaultConstructor { Constructor.name = "FRY", Constructor.semantics = "Western Frisian" }
                                  , defaultConstructor { Constructor.name = "FUL", Constructor.semantics = "Fulah" }
                                  , defaultConstructor { Constructor.name = "FUR", Constructor.semantics = "Friulian" }
                                  , defaultConstructor { Constructor.name = "GAA", Constructor.semantics = "Ga" }
                                  , defaultConstructor { Constructor.name = "GAY", Constructor.semantics = "Gayo" }
                                  , defaultConstructor { Constructor.name = "GBA", Constructor.semantics = "Gbaya" }
                                  , defaultConstructor { Constructor.name = "GEM", Constructor.semantics = "Germanic languages" }
                                  , defaultConstructor { Constructor.name = "GEZ", Constructor.semantics = "Geez" }
                                  , defaultConstructor { Constructor.name = "GIL", Constructor.semantics = "Gilbertese" }
                                  , defaultConstructor { Constructor.name = "GLA", Constructor.semantics = "Gaelic; Scottish Gaelic" }
                                  , defaultConstructor { Constructor.name = "GLE", Constructor.semantics = "Irish" }
                                  , defaultConstructor { Constructor.name = "GLG", Constructor.semantics = "Galician" }
                                  , defaultConstructor { Constructor.name = "GLV", Constructor.semantics = "Manx" }
                                  , defaultConstructor { Constructor.name = "GMH", Constructor.semantics = "German, Middle High (ca.1050-1500)" }
                                  , defaultConstructor { Constructor.name = "GOH", Constructor.semantics = "German, Old High (ca.750-1050)" }
                                  , defaultConstructor { Constructor.name = "GON", Constructor.semantics = "Gondi" }
                                  , defaultConstructor { Constructor.name = "GOR", Constructor.semantics = "Gorontalo" }
                                  , defaultConstructor { Constructor.name = "GOT", Constructor.semantics = "Gothic" }
                                  , defaultConstructor { Constructor.name = "GRB", Constructor.semantics = "Grebo" }
                                  , defaultConstructor { Constructor.name = "GRC", Constructor.semantics = "Greek, Ancient (to 1453)" }
                                  , defaultConstructor { Constructor.name = "GRN", Constructor.semantics = "Guarani" }
                                  , defaultConstructor { Constructor.name = "GSW", Constructor.semantics = "Swiss German; Alemannic; Alsatian" }
                                  , defaultConstructor { Constructor.name = "GUJ", Constructor.semantics = "Gujarati" }
                                  , defaultConstructor { Constructor.name = "GWI", Constructor.semantics = "Gwich'in" }
                                  , defaultConstructor { Constructor.name = "HAI", Constructor.semantics = "Haida" }
                                  , defaultConstructor { Constructor.name = "HAT", Constructor.semantics = "Haitian; Haitian Creole" }
                                  , defaultConstructor { Constructor.name = "HAU", Constructor.semantics = "Hausa" }
                                  , defaultConstructor { Constructor.name = "HAW", Constructor.semantics = "Hawaiian" }
                                  , defaultConstructor { Constructor.name = "HEB", Constructor.semantics = "Hebrew" }
                                  , defaultConstructor { Constructor.name = "HER", Constructor.semantics = "Herero" }
                                  , defaultConstructor { Constructor.name = "HIL", Constructor.semantics = "Hiligaynon" }
                                  , defaultConstructor { Constructor.name = "HIM", Constructor.semantics = "Himachali languages; Western Pahari languages" }
                                  , defaultConstructor { Constructor.name = "HIN", Constructor.semantics = "Hindi" }
                                  , defaultConstructor { Constructor.name = "HIT", Constructor.semantics = "Hittite" }
                                  , defaultConstructor { Constructor.name = "HMN", Constructor.semantics = "Hmong; Mong" }
                                  , defaultConstructor { Constructor.name = "HMO", Constructor.semantics = "Hiri Motu" }
                                  , defaultConstructor { Constructor.name = "HRV", Constructor.semantics = "Croatian" }
                                  , defaultConstructor { Constructor.name = "HSB", Constructor.semantics = "Upper Sorbian" }
                                  , defaultConstructor { Constructor.name = "HUN", Constructor.semantics = "Hungarian" }
                                  , defaultConstructor { Constructor.name = "HUP", Constructor.semantics = "Hupa" }
                                  , defaultConstructor { Constructor.name = "HYE", Constructor.semantics = "Armenian" }
                                  , defaultConstructor { Constructor.name = "IBA", Constructor.semantics = "Iban" }
                                  , defaultConstructor { Constructor.name = "IBO", Constructor.semantics = "Igbo" }
                                  , defaultConstructor { Constructor.name = "IDO", Constructor.semantics = "Ido" }
                                  , defaultConstructor { Constructor.name = "III", Constructor.semantics = "Sichuan Yi; Nuosu" }
                                  , defaultConstructor { Constructor.name = "IJO", Constructor.semantics = "Ijo languages" }
                                  , defaultConstructor { Constructor.name = "IKU", Constructor.semantics = "Inuktitut" }
                                  , defaultConstructor { Constructor.name = "ILE", Constructor.semantics = "Interlingue; Occidental" }
                                  , defaultConstructor { Constructor.name = "ILO", Constructor.semantics = "Iloko" }
                                  , defaultConstructor { Constructor.name = "INA", Constructor.semantics = "Interlingua (International Auxiliary Language Association)" }
                                  , defaultConstructor { Constructor.name = "INC", Constructor.semantics = "Indic languages" }
                                  , defaultConstructor { Constructor.name = "IND", Constructor.semantics = "Indonesian" }
                                  , defaultConstructor { Constructor.name = "INE", Constructor.semantics = "Indo-European languages" }
                                  , defaultConstructor { Constructor.name = "INH", Constructor.semantics = "Ingush" }
                                  , defaultConstructor { Constructor.name = "IPK", Constructor.semantics = "Inupiaq" }
                                  , defaultConstructor { Constructor.name = "IRA", Constructor.semantics = "Iranian languages" }
                                  , defaultConstructor { Constructor.name = "IRO", Constructor.semantics = "Iroquoian languages" }
                                  , defaultConstructor { Constructor.name = "ISL", Constructor.semantics = "Icelandic" }
                                  , defaultConstructor { Constructor.name = "ITA", Constructor.semantics = "Italian" }
                                  , defaultConstructor { Constructor.name = "JAV", Constructor.semantics = "Javanese" }
                                  , defaultConstructor { Constructor.name = "JBO", Constructor.semantics = "Lojban" }
                                  , defaultConstructor { Constructor.name = "JPN", Constructor.semantics = "Japanese" }
                                  , defaultConstructor { Constructor.name = "JPR", Constructor.semantics = "Judeo-Persian" }
                                  , defaultConstructor { Constructor.name = "JRB", Constructor.semantics = "Judeo-Arabic" }
                                  , defaultConstructor { Constructor.name = "KAA", Constructor.semantics = "Kara-Kalpak" }
                                  , defaultConstructor { Constructor.name = "KAB", Constructor.semantics = "Kabyle" }
                                  , defaultConstructor { Constructor.name = "KAC", Constructor.semantics = "Kachin; Jingpho" }
                                  , defaultConstructor { Constructor.name = "KAL", Constructor.semantics = "Kalaallisut; Greenlandic" }
                                  , defaultConstructor { Constructor.name = "KAM", Constructor.semantics = "Kamba" }
                                  , defaultConstructor { Constructor.name = "KAN", Constructor.semantics = "Kannada" }
                                  , defaultConstructor { Constructor.name = "KAR", Constructor.semantics = "Karen languages" }
                                  , defaultConstructor { Constructor.name = "KAS", Constructor.semantics = "Kashmiri" }
                                  , defaultConstructor { Constructor.name = "KAT", Constructor.semantics = "Georgian" }
                                  , defaultConstructor { Constructor.name = "KAU", Constructor.semantics = "Kanuri" }
                                  , defaultConstructor { Constructor.name = "KAW", Constructor.semantics = "Kawi" }
                                  , defaultConstructor { Constructor.name = "KAZ", Constructor.semantics = "Kazakh" }
                                  , defaultConstructor { Constructor.name = "KBD", Constructor.semantics = "Kabardian" }
                                  , defaultConstructor { Constructor.name = "KHA", Constructor.semantics = "Khasi" }
                                  , defaultConstructor { Constructor.name = "KHI", Constructor.semantics = "Khoisan languages" }
                                  , defaultConstructor { Constructor.name = "KHM", Constructor.semantics = "Central Khmer" }
                                  , defaultConstructor { Constructor.name = "KHO", Constructor.semantics = "Khotanese; Sakan" }
                                  , defaultConstructor { Constructor.name = "KIK", Constructor.semantics = "Kikuyu; Gikuyu" }
                                  , defaultConstructor { Constructor.name = "KIN", Constructor.semantics = "Kinyarwanda" }
                                  , defaultConstructor { Constructor.name = "KIR", Constructor.semantics = "Kirghiz; Kyrgyz" }
                                  , defaultConstructor { Constructor.name = "KMB", Constructor.semantics = "Kimbundu" }
                                  , defaultConstructor { Constructor.name = "KOK", Constructor.semantics = "Konkani" }
                                  , defaultConstructor { Constructor.name = "KOM", Constructor.semantics = "Komi" }
                                  , defaultConstructor { Constructor.name = "KON", Constructor.semantics = "Kongo" }
                                  , defaultConstructor { Constructor.name = "KOR", Constructor.semantics = "Korean" }
                                  , defaultConstructor { Constructor.name = "KOS", Constructor.semantics = "Kosraean" }
                                  , defaultConstructor { Constructor.name = "KPE", Constructor.semantics = "Kpelle" }
                                  , defaultConstructor { Constructor.name = "KRC", Constructor.semantics = "Karachay-Balkar" }
                                  , defaultConstructor { Constructor.name = "KRL", Constructor.semantics = "Karelian" }
                                  , defaultConstructor { Constructor.name = "KRO", Constructor.semantics = "Kru languages" }
                                  , defaultConstructor { Constructor.name = "KRU", Constructor.semantics = "Kurukh" }
                                  , defaultConstructor { Constructor.name = "KUA", Constructor.semantics = "Kuanyama; Kwanyama" }
                                  , defaultConstructor { Constructor.name = "KUM", Constructor.semantics = "Kumyk" }
                                  , defaultConstructor { Constructor.name = "KUR", Constructor.semantics = "Kurdish" }
                                  , defaultConstructor { Constructor.name = "KUT", Constructor.semantics = "Kutenai" }
                                  , defaultConstructor { Constructor.name = "LAD", Constructor.semantics = "Ladino" }
                                  , defaultConstructor { Constructor.name = "LAH", Constructor.semantics = "Lahnda" }
                                  , defaultConstructor { Constructor.name = "LAM", Constructor.semantics = "Lamba" }
                                  , defaultConstructor { Constructor.name = "LAO", Constructor.semantics = "Lao" }
                                  , defaultConstructor { Constructor.name = "LAT", Constructor.semantics = "Latin" }
                                  , defaultConstructor { Constructor.name = "LAV", Constructor.semantics = "Latvian" }
                                  , defaultConstructor { Constructor.name = "LEZ", Constructor.semantics = "Lezghian" }
                                  , defaultConstructor { Constructor.name = "LIM", Constructor.semantics = "Limburgan; Limburger; Limburgish" }
                                  , defaultConstructor { Constructor.name = "LIN", Constructor.semantics = "Lingala" }
                                  , defaultConstructor { Constructor.name = "LIT", Constructor.semantics = "Lithuanian" }
                                  , defaultConstructor { Constructor.name = "LOL", Constructor.semantics = "Mongo" }
                                  , defaultConstructor { Constructor.name = "LOZ", Constructor.semantics = "Lozi" }
                                  , defaultConstructor { Constructor.name = "LTZ", Constructor.semantics = "Luxembourgish; Letzeburgesch" }
                                  , defaultConstructor { Constructor.name = "LUA", Constructor.semantics = "Luba-Lulua" }
                                  , defaultConstructor { Constructor.name = "LUB", Constructor.semantics = "Luba-Katanga" }
                                  , defaultConstructor { Constructor.name = "LUG", Constructor.semantics = "Ganda" }
                                  , defaultConstructor { Constructor.name = "LUI", Constructor.semantics = "Luiseno" }
                                  , defaultConstructor { Constructor.name = "LUN", Constructor.semantics = "Lunda" }
                                  , defaultConstructor { Constructor.name = "LUO", Constructor.semantics = "Luo (Kenya and Tanzania)" }
                                  , defaultConstructor { Constructor.name = "LUS", Constructor.semantics = "Lushai" }
                                  , defaultConstructor { Constructor.name = "MAD", Constructor.semantics = "Madurese" }
                                  , defaultConstructor { Constructor.name = "MAG", Constructor.semantics = "Magahi" }
                                  , defaultConstructor { Constructor.name = "MAH", Constructor.semantics = "Marshallese" }
                                  , defaultConstructor { Constructor.name = "MAI", Constructor.semantics = "Maithili" }
                                  , defaultConstructor { Constructor.name = "MAK", Constructor.semantics = "Makasar" }
                                  , defaultConstructor { Constructor.name = "MAL", Constructor.semantics = "Malayalam" }
                                  , defaultConstructor { Constructor.name = "MAN", Constructor.semantics = "Mandingo" }
                                  , defaultConstructor { Constructor.name = "MAP", Constructor.semantics = "Austronesian languages" }
                                  , defaultConstructor { Constructor.name = "MAR", Constructor.semantics = "Marathi" }
                                  , defaultConstructor { Constructor.name = "MAS", Constructor.semantics = "Masai" }
                                  , defaultConstructor { Constructor.name = "MDF", Constructor.semantics = "Moksha" }
                                  , defaultConstructor { Constructor.name = "MDR", Constructor.semantics = "Mandar" }
                                  , defaultConstructor { Constructor.name = "MEN", Constructor.semantics = "Mende" }
                                  , defaultConstructor { Constructor.name = "MGA", Constructor.semantics = "Irish, Middle (900-1200)" }
                                  , defaultConstructor { Constructor.name = "MIC", Constructor.semantics = "Mi'kmaq; Micmac" }
                                  , defaultConstructor { Constructor.name = "MIN", Constructor.semantics = "Minangkabau" }
                                  , defaultConstructor { Constructor.name = "MIS", Constructor.semantics = "Uncoded languages" }
                                  , defaultConstructor { Constructor.name = "MKD", Constructor.semantics = "Macedonian" }
                                  , defaultConstructor { Constructor.name = "MKH", Constructor.semantics = "Mon-Khmer languages" }
                                  , defaultConstructor { Constructor.name = "MLG", Constructor.semantics = "Malagasy" }
                                  , defaultConstructor { Constructor.name = "MLT", Constructor.semantics = "Maltese" }
                                  , defaultConstructor { Constructor.name = "MNC", Constructor.semantics = "Manchu" }
                                  , defaultConstructor { Constructor.name = "MNI", Constructor.semantics = "Manipuri" }
                                  , defaultConstructor { Constructor.name = "MNO", Constructor.semantics = "Manobo languages" }
                                  , defaultConstructor { Constructor.name = "MOH", Constructor.semantics = "Mohawk" }
                                  , defaultConstructor { Constructor.name = "MON", Constructor.semantics = "Mongolian" }
                                  , defaultConstructor { Constructor.name = "MOS", Constructor.semantics = "Mossi" }
                                  , defaultConstructor { Constructor.name = "MRI", Constructor.semantics = "Maori" }
                                  , defaultConstructor { Constructor.name = "MSA", Constructor.semantics = "Malay" }
                                  , defaultConstructor { Constructor.name = "MUL", Constructor.semantics = "Multiple languages" }
                                  , defaultConstructor { Constructor.name = "MUN", Constructor.semantics = "Munda languages" }
                                  , defaultConstructor { Constructor.name = "MUS", Constructor.semantics = "Creek" }
                                  , defaultConstructor { Constructor.name = "MWL", Constructor.semantics = "Mirandese" }
                                  , defaultConstructor { Constructor.name = "MWR", Constructor.semantics = "Marwari" }
                                  , defaultConstructor { Constructor.name = "MYA", Constructor.semantics = "Burmese" }
                                  , defaultConstructor { Constructor.name = "MYN", Constructor.semantics = "Mayan languages" }
                                  , defaultConstructor { Constructor.name = "MYV", Constructor.semantics = "Erzya" }
                                  , defaultConstructor { Constructor.name = "NAH", Constructor.semantics = "Nahuatl languages" }
                                  , defaultConstructor { Constructor.name = "NAI", Constructor.semantics = "North American Indian languages" }
                                  , defaultConstructor { Constructor.name = "NAP", Constructor.semantics = "Neapolitan" }
                                  , defaultConstructor { Constructor.name = "NAU", Constructor.semantics = "Nauru" }
                                  , defaultConstructor { Constructor.name = "NAV", Constructor.semantics = "Navajo; Navaho" }
                                  , defaultConstructor { Constructor.name = "NBL", Constructor.semantics = "Ndebele, South; South Ndebele" }
                                  , defaultConstructor { Constructor.name = "NDE", Constructor.semantics = "Ndebele, North; North Ndebele" }
                                  , defaultConstructor { Constructor.name = "NDO", Constructor.semantics = "Ndonga" }
                                  , defaultConstructor { Constructor.name = "NDS", Constructor.semantics = "Low German; Low Saxon; German, Low; Saxon, Low" }
                                  , defaultConstructor { Constructor.name = "NEP", Constructor.semantics = "Nepali" }
                                  , defaultConstructor { Constructor.name = "NEW", Constructor.semantics = "Nepal Bhasa; Newari" }
                                  , defaultConstructor { Constructor.name = "NIA", Constructor.semantics = "Nias" }
                                  , defaultConstructor { Constructor.name = "NIC", Constructor.semantics = "Niger-Kordofanian languages" }
                                  , defaultConstructor { Constructor.name = "NIU", Constructor.semantics = "Niuean" }
                                  , defaultConstructor { Constructor.name = "NLD", Constructor.semantics = "Dutch; Flemish" }
                                  , defaultConstructor { Constructor.name = "NNO", Constructor.semantics = "Norwegian Nynorsk; Nynorsk, Norwegian" }
                                  , defaultConstructor { Constructor.name = "NOB", Constructor.semantics = "Bokmål, Norwegian; Norwegian Bokmål" }
                                  , defaultConstructor { Constructor.name = "NOG", Constructor.semantics = "Nogai" }
                                  , defaultConstructor { Constructor.name = "NON", Constructor.semantics = "Norse, Old" }
                                  , defaultConstructor { Constructor.name = "NOR", Constructor.semantics = "Norwegian" }
                                  , defaultConstructor { Constructor.name = "NQO", Constructor.semantics = "N'Ko" }
                                  , defaultConstructor { Constructor.name = "NSO", Constructor.semantics = "Pedi; Sepedi; Northern Sotho" }
                                  , defaultConstructor { Constructor.name = "NUB", Constructor.semantics = "Nubian languages" }
                                  , defaultConstructor { Constructor.name = "NWC", Constructor.semantics = "Classical Newari; Old Newari; Classical Nepal Bhasa" }
                                  , defaultConstructor { Constructor.name = "NYA", Constructor.semantics = "Chichewa; Chewa; Nyanja" }
                                  , defaultConstructor { Constructor.name = "NYM", Constructor.semantics = "Nyamwezi" }
                                  , defaultConstructor { Constructor.name = "NYN", Constructor.semantics = "Nyankole" }
                                  , defaultConstructor { Constructor.name = "NYO", Constructor.semantics = "Nyoro" }
                                  , defaultConstructor { Constructor.name = "NZI", Constructor.semantics = "Nzima" }
                                  , defaultConstructor { Constructor.name = "OCI", Constructor.semantics = "Occitan (post 1500)" }
                                  , defaultConstructor { Constructor.name = "OJI", Constructor.semantics = "Ojibwa" }
                                  , defaultConstructor { Constructor.name = "ORI", Constructor.semantics = "Oriya" }
                                  , defaultConstructor { Constructor.name = "ORM", Constructor.semantics = "Oromo" }
                                  , defaultConstructor { Constructor.name = "OSA", Constructor.semantics = "Osage" }
                                  , defaultConstructor { Constructor.name = "OSS", Constructor.semantics = "Ossetian; Ossetic" }
                                  , defaultConstructor { Constructor.name = "OTA", Constructor.semantics = "Turkish, Ottoman (1500-1928)" }
                                  , defaultConstructor { Constructor.name = "OTO", Constructor.semantics = "Otomian languages" }
                                  , defaultConstructor { Constructor.name = "PAA", Constructor.semantics = "Papuan languages" }
                                  , defaultConstructor { Constructor.name = "PAG", Constructor.semantics = "Pangasinan" }
                                  , defaultConstructor { Constructor.name = "PAL", Constructor.semantics = "Pahlavi" }
                                  , defaultConstructor { Constructor.name = "PAM", Constructor.semantics = "Pampanga; Kapampangan" }
                                  , defaultConstructor { Constructor.name = "PAN", Constructor.semantics = "Panjabi; Punjabi" }
                                  , defaultConstructor { Constructor.name = "PAP", Constructor.semantics = "Papiamento" }
                                  , defaultConstructor { Constructor.name = "PAU", Constructor.semantics = "Palauan" }
                                  , defaultConstructor { Constructor.name = "PEO", Constructor.semantics = "Persian, Old (ca.600-400 B.C.)" }
                                  , defaultConstructor { Constructor.name = "PHI", Constructor.semantics = "Philippine languages" }
                                  , defaultConstructor { Constructor.name = "PHN", Constructor.semantics = "Phoenician" }
                                  , defaultConstructor { Constructor.name = "PLI", Constructor.semantics = "Pali" }
                                  , defaultConstructor { Constructor.name = "POL", Constructor.semantics = "Polish" }
                                  , defaultConstructor { Constructor.name = "PON", Constructor.semantics = "Pohnpeian" }
                                  , defaultConstructor { Constructor.name = "POR", Constructor.semantics = "Portuguese" }
                                  , defaultConstructor { Constructor.name = "PRA", Constructor.semantics = "Prakrit languages" }
                                  , defaultConstructor { Constructor.name = "PRO", Constructor.semantics = "Provençal, Old (to 1500);Occitan, Old (to 1500)" }
                                  , defaultConstructor { Constructor.name = "PUS", Constructor.semantics = "Pushto; Pashto" }
                                  , defaultConstructor { Constructor.name = "QAA", Constructor.semantics = "QTZ Reserved for local use" }
                                  , defaultConstructor { Constructor.name = "QUE", Constructor.semantics = "Quechua" }
                                  , defaultConstructor { Constructor.name = "RAJ", Constructor.semantics = "Rajasthani" }
                                  , defaultConstructor { Constructor.name = "RAP", Constructor.semantics = "Rapanui" }
                                  , defaultConstructor { Constructor.name = "RAR", Constructor.semantics = "Rarotongan; Cook Islands Maori" }
                                  , defaultConstructor { Constructor.name = "ROA", Constructor.semantics = "Romance languages" }
                                  , defaultConstructor { Constructor.name = "ROH", Constructor.semantics = "Romansh" }
                                  , defaultConstructor { Constructor.name = "ROM", Constructor.semantics = "Romany" }
                                  , defaultConstructor { Constructor.name = "RON", Constructor.semantics = "Romanian; Moldavian; Moldovan" }
                                  , defaultConstructor { Constructor.name = "RUN", Constructor.semantics = "Rundi" }
                                  , defaultConstructor { Constructor.name = "RUP", Constructor.semantics = "Aromanian; Arumanian; Macedo-Romanian" }
                                  , defaultConstructor { Constructor.name = "RUS", Constructor.semantics = "Russian" }
                                  , defaultConstructor { Constructor.name = "SAD", Constructor.semantics = "Sandawe" }
                                  , defaultConstructor { Constructor.name = "SAG", Constructor.semantics = "Sango" }
                                  , defaultConstructor { Constructor.name = "SAH", Constructor.semantics = "Yakut" }
                                  , defaultConstructor { Constructor.name = "SAI", Constructor.semantics = "South American Indian languages" }
                                  , defaultConstructor { Constructor.name = "SAL", Constructor.semantics = "Salishan languages" }
                                  , defaultConstructor { Constructor.name = "SAM", Constructor.semantics = "Samaritan Aramaic" }
                                  , defaultConstructor { Constructor.name = "SAN", Constructor.semantics = "Sanskrit" }
                                  , defaultConstructor { Constructor.name = "SAS", Constructor.semantics = "Sasak" }
                                  , defaultConstructor { Constructor.name = "SAT", Constructor.semantics = "Santali" }
                                  , defaultConstructor { Constructor.name = "SCN", Constructor.semantics = "Sicilian" }
                                  , defaultConstructor { Constructor.name = "SCO", Constructor.semantics = "Scots" }
                                  , defaultConstructor { Constructor.name = "SEL", Constructor.semantics = "Selkup" }
                                  , defaultConstructor { Constructor.name = "SEM", Constructor.semantics = "Semitic languages" }
                                  , defaultConstructor { Constructor.name = "SGA", Constructor.semantics = "Irish, Old (to 900)" }
                                  , defaultConstructor { Constructor.name = "SGN", Constructor.semantics = "Sign Languages" }
                                  , defaultConstructor { Constructor.name = "SHN", Constructor.semantics = "Shan" }
                                  , defaultConstructor { Constructor.name = "SID", Constructor.semantics = "Sidamo" }
                                  , defaultConstructor { Constructor.name = "SIN", Constructor.semantics = "Sinhala; Sinhalese" }
                                  , defaultConstructor { Constructor.name = "SIO", Constructor.semantics = "Siouan languages" }
                                  , defaultConstructor { Constructor.name = "SIT", Constructor.semantics = "Sino-Tibetan languages" }
                                  , defaultConstructor { Constructor.name = "SLA", Constructor.semantics = "Slavic languages" }
                                  , defaultConstructor { Constructor.name = "SLK", Constructor.semantics = "Slovak" }
                                  , defaultConstructor { Constructor.name = "SLV", Constructor.semantics = "Slovenian" }
                                  , defaultConstructor { Constructor.name = "SMA", Constructor.semantics = "Southern Sami" }
                                  , defaultConstructor { Constructor.name = "SME", Constructor.semantics = "Northern Sami" }
                                  , defaultConstructor { Constructor.name = "SMI", Constructor.semantics = "Sami languages" }
                                  , defaultConstructor { Constructor.name = "SMJ", Constructor.semantics = "Lule Sami" }
                                  , defaultConstructor { Constructor.name = "SMN", Constructor.semantics = "Inari Sami" }
                                  , defaultConstructor { Constructor.name = "SMO", Constructor.semantics = "Samoan" }
                                  , defaultConstructor { Constructor.name = "SMS", Constructor.semantics = "Skolt Sami" }
                                  , defaultConstructor { Constructor.name = "SNA", Constructor.semantics = "Shona" }
                                  , defaultConstructor { Constructor.name = "SND", Constructor.semantics = "Sindhi" }
                                  , defaultConstructor { Constructor.name = "SNK", Constructor.semantics = "Soninke" }
                                  , defaultConstructor { Constructor.name = "SOG", Constructor.semantics = "Sogdian" }
                                  , defaultConstructor { Constructor.name = "SOM", Constructor.semantics = "Somali" }
                                  , defaultConstructor { Constructor.name = "SON", Constructor.semantics = "Songhai languages" }
                                  , defaultConstructor { Constructor.name = "SOT", Constructor.semantics = "Sotho, Southern" }
                                  , defaultConstructor { Constructor.name = "SPA", Constructor.semantics = "Spanish; Castilian" }
                                  , defaultConstructor { Constructor.name = "SQI", Constructor.semantics = "Albanian" }
                                  , defaultConstructor { Constructor.name = "SRD", Constructor.semantics = "Sardinian" }
                                  , defaultConstructor { Constructor.name = "SRN", Constructor.semantics = "Sranan Tongo" }
                                  , defaultConstructor { Constructor.name = "SRP", Constructor.semantics = "Serbian" }
                                  , defaultConstructor { Constructor.name = "SRR", Constructor.semantics = "Serer" }
                                  , defaultConstructor { Constructor.name = "SSA", Constructor.semantics = "Nilo-Saharan languages" }
                                  , defaultConstructor { Constructor.name = "SSW", Constructor.semantics = "Swati" }
                                  , defaultConstructor { Constructor.name = "SUK", Constructor.semantics = "Sukuma" }
                                  , defaultConstructor { Constructor.name = "SUN", Constructor.semantics = "Sundanese" }
                                  , defaultConstructor { Constructor.name = "SUS", Constructor.semantics = "Susu" }
                                  , defaultConstructor { Constructor.name = "SUX", Constructor.semantics = "Sumerian" }
                                  , defaultConstructor { Constructor.name = "SWA", Constructor.semantics = "Swahili" }
                                  , defaultConstructor { Constructor.name = "SWE", Constructor.semantics = "Swedish" }
                                  , defaultConstructor { Constructor.name = "SYC", Constructor.semantics = "Classical Syriac" }
                                  , defaultConstructor { Constructor.name = "SYR", Constructor.semantics = "Syriac" }
                                  , defaultConstructor { Constructor.name = "TAH", Constructor.semantics = "Tahitian" }
                                  , defaultConstructor { Constructor.name = "TAI", Constructor.semantics = "Tai languages" }
                                  , defaultConstructor { Constructor.name = "TAM", Constructor.semantics = "Tamil" }
                                  , defaultConstructor { Constructor.name = "TAT", Constructor.semantics = "Tatar" }
                                  , defaultConstructor { Constructor.name = "TEL", Constructor.semantics = "Telugu" }
                                  , defaultConstructor { Constructor.name = "TEM", Constructor.semantics = "Timne" }
                                  , defaultConstructor { Constructor.name = "TER", Constructor.semantics = "Tereno" }
                                  , defaultConstructor { Constructor.name = "TET", Constructor.semantics = "Tetum" }
                                  , defaultConstructor { Constructor.name = "TGK", Constructor.semantics = "Tajik" }
                                  , defaultConstructor { Constructor.name = "TGL", Constructor.semantics = "Tagalog" }
                                  , defaultConstructor { Constructor.name = "THA", Constructor.semantics = "Thai" }
                                  , defaultConstructor { Constructor.name = "TIG", Constructor.semantics = "Tigre" }
                                  , defaultConstructor { Constructor.name = "TIR", Constructor.semantics = "Tigrinya" }
                                  , defaultConstructor { Constructor.name = "TIV", Constructor.semantics = "Tiv" }
                                  , defaultConstructor { Constructor.name = "TKL", Constructor.semantics = "Tokelau" }
                                  , defaultConstructor { Constructor.name = "TLH", Constructor.semantics = "Klingon; tlhIngan-Hol" }
                                  , defaultConstructor { Constructor.name = "TLI", Constructor.semantics = "Tlingit" }
                                  , defaultConstructor { Constructor.name = "TMH", Constructor.semantics = "Tamashek" }
                                  , defaultConstructor { Constructor.name = "TOG", Constructor.semantics = "Tonga (Nyasa)" }
                                  , defaultConstructor { Constructor.name = "TON", Constructor.semantics = "Tonga (Tonga Islands)" }
                                  , defaultConstructor { Constructor.name = "TPI", Constructor.semantics = "Tok Pisin" }
                                  , defaultConstructor { Constructor.name = "TSI", Constructor.semantics = "Tsimshian" }
                                  , defaultConstructor { Constructor.name = "TSN", Constructor.semantics = "Tswana" }
                                  , defaultConstructor { Constructor.name = "TSO", Constructor.semantics = "Tsonga" }
                                  , defaultConstructor { Constructor.name = "TUK", Constructor.semantics = "Turkmen" }
                                  , defaultConstructor { Constructor.name = "TUM", Constructor.semantics = "Tumbuka" }
                                  , defaultConstructor { Constructor.name = "TUP", Constructor.semantics = "Tupi languages" }
                                  , defaultConstructor { Constructor.name = "TUR", Constructor.semantics = "Turkish" }
                                  , defaultConstructor { Constructor.name = "TUT", Constructor.semantics = "Altaic languages" }
                                  , defaultConstructor { Constructor.name = "TVL", Constructor.semantics = "Tuvalu" }
                                  , defaultConstructor { Constructor.name = "TWI", Constructor.semantics = "Twi" }
                                  , defaultConstructor { Constructor.name = "TYV", Constructor.semantics = "Tuvinian" }
                                  , defaultConstructor { Constructor.name = "UDM", Constructor.semantics = "Udmurt" }
                                  , defaultConstructor { Constructor.name = "UGA", Constructor.semantics = "Ugaritic" }
                                  , defaultConstructor { Constructor.name = "UIG", Constructor.semantics = "Uighur; Uyghur" }
                                  , defaultConstructor { Constructor.name = "UKR", Constructor.semantics = "Ukrainian" }
                                  , defaultConstructor { Constructor.name = "UMB", Constructor.semantics = "Umbundu" }
                                  , defaultConstructor { Constructor.name = "UND", Constructor.semantics = "Undetermined" }
                                  , defaultConstructor { Constructor.name = "URD", Constructor.semantics = "Urdu" }
                                  , defaultConstructor { Constructor.name = "UZB", Constructor.semantics = "Uzbek" }
                                  , defaultConstructor { Constructor.name = "VAI", Constructor.semantics = "Vai" }
                                  , defaultConstructor { Constructor.name = "VEN", Constructor.semantics = "Venda" }
                                  , defaultConstructor { Constructor.name = "VIE", Constructor.semantics = "Vietnamese" }
                                  , defaultConstructor { Constructor.name = "VOL", Constructor.semantics = "Volapük" }
                                  , defaultConstructor { Constructor.name = "VOT", Constructor.semantics = "Votic" }
                                  , defaultConstructor { Constructor.name = "WAK", Constructor.semantics = "Wakashan languages" }
                                  , defaultConstructor { Constructor.name = "WAL", Constructor.semantics = "Wolaitta; Wolaytta" }
                                  , defaultConstructor { Constructor.name = "WAR", Constructor.semantics = "Waray" }
                                  , defaultConstructor { Constructor.name = "WAS", Constructor.semantics = "Washo" }
                                  , defaultConstructor { Constructor.name = "WEN", Constructor.semantics = "Sorbian languages" }
                                  , defaultConstructor { Constructor.name = "WLN", Constructor.semantics = "Walloon" }
                                  , defaultConstructor { Constructor.name = "WOL", Constructor.semantics = "Wolof" }
                                  , defaultConstructor { Constructor.name = "XAL", Constructor.semantics = "Kalmyk; Oirat" }
                                  , defaultConstructor { Constructor.name = "XHO", Constructor.semantics = "Xhosa" }
                                  , defaultConstructor { Constructor.name = "YAO", Constructor.semantics = "Yao" }
                                  , defaultConstructor { Constructor.name = "YAP", Constructor.semantics = "Yapese" }
                                  , defaultConstructor { Constructor.name = "YID", Constructor.semantics = "Yiddish" }
                                  , defaultConstructor { Constructor.name = "YOR", Constructor.semantics = "Yoruba" }
                                  , defaultConstructor { Constructor.name = "YPK", Constructor.semantics = "Yupik languages" }
                                  , defaultConstructor { Constructor.name = "ZAP", Constructor.semantics = "Zapotec" }
                                  , defaultConstructor { Constructor.name = "ZBL", Constructor.semantics = "Blissymbols; Blissymbolics; Bliss" }
                                  , defaultConstructor { Constructor.name = "ZEN", Constructor.semantics = "Zenaga" }
                                  , defaultConstructor { Constructor.name = "ZHA", Constructor.semantics = "Zhuang; Chuang" }
                                  , defaultConstructor { Constructor.name = "ZHO", Constructor.semantics = "Chinese" }
                                  , defaultConstructor { Constructor.name = "ZND", Constructor.semantics = "Zande languages" }
                                  , defaultConstructor { Constructor.name = "ZUL", Constructor.semantics = "Zulu" }
                                  , defaultConstructor { Constructor.name = "ZUN", Constructor.semantics = "Zuni" }
                                  , defaultConstructor { Constructor.name = "ZXX", Constructor.semantics = "No linguistic content; Not applicable" }
                                  , defaultConstructor { Constructor.name = "ZZA", Constructor.semantics = "Zaza; Dimili; Dimli; Kirdki; Kirmanjki; Zazaki" }
                                 ]
          }}               


t99       = dt {
             identifier   = "e959d910-2edb-11e0-91fa-0800200c9a66"
           , author       = Just personClemens
           , name         = "SimpleDialog"
           , structure    = v1 $ (dt' :: Type (Succ Zero))
           { semantics    = "One conversation between an arbitrary number of speakers taking turns (no overlap)"
           , constructors = Just [
                                   defaultConstructor { 
                                     Constructor.name = "SimpleDialog"
                                    ,Constructor.fields = [ defaultField {
                                                                   Field.name = "dialoguemetadata" 
                                                                ,  Field.type_ = DataType "0c761f8e-757e-4ea7-9d24-2a01136452d2" --SimpleMeta  
                                                                 }
                                                         , defaultField {
                                                                   Field.name = "turns" 
                                                                ,  Field.type_ = list $ (Application
                                                                                       (DataType "dd9cf67a-3e2b-488d-aeb9-df9c29566a99") -- Turn
                                                                                       (Variable First)
                                                                                      )
                                                                 }
                                                         ]
                                   }           
                                 ]
          }} 
            
t100       = dt {
             identifier   = "0c761f8e-757e-4ea7-9d24-2a01136452d2"
           , author       = Just personClemens
           , name         = "SimpleMeta"
           , structure    = v0 $ (dt' :: Type Zero)
           { semantics    = "The most basic information about something: Its name, and an unstructured comment about it."
           , constructors = Just [
                                   defaultConstructor { 
                                     Constructor.name = "SimpleMeta"
                                    ,Constructor.fields = [ defaultField {
                                                                   Field.name = "name" 
                                                                ,  Field.type_ = DataType "4f7db06c439541658a09689d3e7dd909"   -- Text
                                                                 }
                                                         , defaultField {
                                                                   Field.name = "comments" 
                                                                ,  Field.type_ = DataType "4f7db06c439541658a09689d3e7dd909"
                                                                 }
                                                         ]
                                   }           
                                 ]
          }}           
          

t101       = dt {
             identifier   = "dd9cf67a-3e2b-488d-aeb9-df9c29566a99"
           , author       = Just personClemens
           , name         = "Turn"
           , structure    = v1 $ (dt' :: Type (Succ Zero))
           { semantics    = "one continuous utterance of one speaker. "
           , constructors = Just [
                                   defaultConstructor { 
                                     Constructor.name = "Turn"
                                    ,Constructor.fields = [ defaultField {
                                                                   Field.name = "who" 
                                                                ,  Field.type_ = DataType "26b9a53370bc4489a322192e2e0416ce"  
                                                                 }
                                                         , defaultField {
                                                                   Field.name = "utterance" 
                                                                ,  Field.type_ = list $ (Variable First)
                                                                                      
                                                                 }
                                                         ]
                                   }           
                                 ]
          }} 

t102       = dt {
             identifier   = "26b9a533-70bc-4489-a322-192e2e0416ce"
           , author       = Just personClemens
           , name         = "SimpleSpeaker"
           , structure    = v0 $ (dt' :: Type Zero)
           { semantics    = "Basic information about one speaker"
           , constructors = Just [
                                   defaultConstructor { 
                                     Constructor.name = "SimpleMeta"
                                    ,Constructor.fields = [ defaultField {
                                                                   Field.name = "name" 
                                                                ,  Field.type_ = DataType "4f7db06c439541658a09689d3e7dd909"   -- Text
                                                                 }
                                                         , defaultField {
                                                                   Field.name = "gender" 
                                                                ,  Field.type_ = DataType "2dbb6df873ad4e4baeb82172074ed042" -- gender
                                                                 }
                                                         ]
                                   }           
                                 ]
          }}   



{--
t59 = dt {
	identifier = "90eceef9-1189-4a18-903b-9cf36eb18e97",
  author       = Just personMikael,
	name = "Permission",
	semantics = "File access permission mask.",
	constructors = Just [
		defaultConstructor {
			Constructor.name = "Permission",
			Constructor.fields = [
				(defaultField :: Field.Field Concrete) {
					Field.name = "read",
					Field.semantics = "Read permission.",
					Field.type_ = DataType "0219c59f732a8ef507215fbdb4cceacd"
				},
				(defaultField :: Field.Field Concrete) {
					Field.name = "write",
					Field.semantics = "Write permission.",
					Field.type_ = DataType "0219c59f732a8ef507215fbdb4cceacd"
				},
				(defaultField :: Field.Field Concrete) {
					Field.name = "exec",
					Field.semantics = "Execute permission.",
					Field.type_ = DataType "0219c59f732a8ef507215fbdb4cceacd"
				}
			]
		}
	]
}

t60     :: TypeDefinition Concrete 
t60 = dt {
	identifier = "add67dbc-2e18-4ffd-aea3-b1e8cb28f7d8",
  author       = Just personMikael,
	name = "UnixTime",
	semantics = "UNIX time stamp.",
	constructors = Just [
		defaultConstructor {
			Constructor.name = "UnixTime",
			Constructor.fields = [
				(defaultField :: Field.Field Concrete) {
					Field.name = "nanoSeconds",
					Field.semantics = "Nanoseconds since the Epoch, 1970-01-01 00:00 +0000.",
					Field.type_ = DataType "ac2e770f2132aced749ec197385ff552"
				}
			]
		}
	]
}

t61     :: TypeDefinition Concrete 
t61 = dt {
	identifier = "41f3d3c6-311b-4f18-a600-758219595871",
  author       = Just personMikael,
	name = "SpecialType",
	semantics = "Special file type.",
	constructors = Just [
		defaultConstructor { Constructor.name = "Character" },
		defaultConstructor { Constructor.name = "Block" }
	]
}

t62     :: TypeDefinition Concrete 
t62 = dt {
	identifier = "fa052506-7ac8-4473-a274-c4bac5ad0cc4",
  author       = Just personMikael,
	name = "IPCType",
	semantics = "IPC object type.",
	constructors = Just [
		defaultConstructor { Constructor.name = "Pipe" },
		defaultConstructor { Constructor.name = "Socket" }
	]
}

t63     :: TypeDefinition (Application Concrete Concrete)
t63 = dt {
	identifier = "027770dd-5134-4ee0-8cd8-faf29e962167",
  author       = Just personMikael,
	name = "File",
	semantics = "File system file.",
	constructors = Just [
		(defaultConstructor :: Constructor.Constructor (Application Concrete Concrete)) {
			Constructor.name = "Directory",
			Constructor.semantics = "File directory.",
			Constructor.fields = [
				(defaultField :: Field.Field  (Application Concrete Concrete)) {
					Field.name = "defacl",
					Field.semantics = "Default permission mask for access control lists.",
					Field.type_ = DataType "90eceef9-1189-4a18-903b-9cf36eb18e97"
				},
				(defaultField :: Field.Field  (Application Concrete Concrete)) {
					Field.name = "entries",
					Field.semantics = "Directory entries.",
					Field.type_ =
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
		(defaultConstructor :: Constructor.Constructor (Application Concrete Concrete)) {
			Constructor.name = "Link",
			Constructor.semantics = "Symbolic link.",
			Constructor.fields = [
				(defaultField :: Field.Field  (Application Concrete Concrete)) {
					Field.name = "target",
					Field.semantics = "Link target.",
					Field.type_ = DataType "5448c6b7-9a08-4b4e-a40d-442c4fd2e125"
				}
			]
		},
		(defaultConstructor :: Constructor.Constructor (Application Concrete Concrete)) {
			Constructor.name = "Special",
			Constructor.semantics = "Special (device) file.",
			Constructor.fields = [
				(defaultField :: Field.Field  (Application Concrete Concrete)) {
					Field.name = "specialType",
					Field.semantics = "Type of special file.",
					Field.type_ = DataType "41f3d3c6-311b-4f18-a600-758219595871"
				},
				(defaultField :: Field.Field  (Application Concrete Concrete)) {
					Field.name = "major",
					Field.semantics = "Major device number.",
					Field.type_ = DataType "62d2d537-1f08-461a-a328-bc06561594f6"
				},
				(defaultField :: Field.Field  (Application Concrete Concrete)) {
					Field.name = "minor",
					Field.semantics = "Minor device number.",
					Field.type_ = DataType "62d2d537-1f08-461a-a328-bc06561594f6"
				}
			]
		},
		(defaultConstructor :: Constructor.Constructor (Application Concrete Concrete)) {
			Constructor.name = "IPC",
			Constructor.semantics = "Inter-process communication object.",
			Constructor.fields = [
				(defaultField :: Field.Field  (Application Concrete Concrete)){
					Field.name = "ipcType",
					Field.semantics = "Type of IPC object.",
					Field.type_ = DataType "fa052506-7ac8-4473-a274-c4bac5ad0cc4"
				}
			]
		},
		(defaultConstructor :: Constructor.Constructor  (Application Concrete Concrete)) {
			Constructor.name = "Regular",
			Constructor.semantics = "Regular file.",
			Constructor.fields = [
				(defaultField :: Field.Field  (Application Concrete Concrete)) {
					Field.name = "content",
					Field.semantics = "File content.",
					Field.type_ = DataType "f9f2f27af0f649b4bc8946c467c3b76a"
				}
			]
		}
	]
}

t64     :: TypeDefinition (Application Concrete Concrete) 
t64 = dt {
	identifier = "4eabdb36-0fbf-4df6-9dcd-33d16dac9516",
  author       = Just personMikael,
	name = "Inode",
	semantics = "File system inode.",
	constructors = Just [
		defaultConstructor {
			Constructor.name = "Inode",
			Constructor.fields = [
				(defaultField :: Field.Field (Application Concrete Concrete)) {
					Field.name = "owner",
					Field.semantics = "File owner.",
					Field.type_ = Variable First
				},
				(defaultField :: Field.Field (Application Concrete Concrete)) {
					Field.name = "group",
					Field.semantics = "File owner group.",
					Field.type_ = Variable First
				},
				(defaultField :: Field.Field (Application Concrete Concrete)) {
					Field.name = "setuid",
					Field.semantics = "Set user ID flag.",
					Field.type_ = DataType "0219c59f732a8ef507215fbdb4cceacd"
				},
				(defaultField  :: Field.Field (Application Concrete Concrete)){
					Field.name = "setgid",
					Field.semantics = "Set group ID flag.",
					Field.type_ = DataType "0219c59f732a8ef507215fbdb4cceacd"
				},
				(defaultField :: Field.Field (Application Concrete Concrete)) {
					Field.name = "sticky",
					Field.semantics = "Sticky bit.",
					Field.type_ = DataType "0219c59f732a8ef507215fbdb4cceacd"
				},
				(defaultField :: Field.Field (Application Concrete Concrete)) {
					Field.name = "uperm",
					Field.semantics = "User access permission mask.",
					Field.type_ = DataType "90eceef9-1189-4a18-903b-9cf36eb18e97"
				},
				(defaultField :: Field.Field (Application Concrete Concrete)) {
					Field.name = "gperm",
					Field.semantics = "Group access permission mask.",
					Field.type_ = DataType "90eceef9-1189-4a18-903b-9cf36eb18e97"
				},
				(defaultField :: Field.Field (Application Concrete Concrete)) {
					Field.name = "operm",
					Field.semantics = "Other access permission mask.",
					Field.type_ = DataType "90eceef9-1189-4a18-903b-9cf36eb18e97"
				},
				(defaultField :: Field.Field (Application Concrete Concrete)) {
					Field.name = "mask",
					Field.semantics = "Maximum permissions to be granted via access control lists.",
					Field.type_ = DataType "90eceef9-1189-4a18-903b-9cf36eb18e97"
				},
				(defaultField :: Field.Field (Application Concrete Concrete)) {
					Field.name = "usracl",
					Field.semantics = "User access control list.",
					Field.type_ =
						(Application
							(Application
								(DataType "43c6cd13-33b0-4fc8-a480-668ecb24768e")
								(Variable First)
							)
							(DataType "90eceef9-1189-4a18-903b-9cf36eb18e97")
						)
				},
				(defaultField :: Field.Field (Application Concrete Concrete)) {
					Field.name = "grpacl",
					Field.semantics = "Group access control list.",
					Field.type_ =
						(Application
							(Application
								(DataType "43c6cd13-33b0-4fc8-a480-668ecb24768e")
								(Variable First)
							)
							(DataType "90eceef9-1189-4a18-903b-9cf36eb18e97")
						)
				},
				(defaultField :: Field.Field (Application Concrete Concrete)) {
					Field.name = "mtime",
					Field.semantics = "Time of last modification.",
					Field.type_ = DataType "add67dbc-2e18-4ffd-aea3-b1e8cb28f7d8"
				},
				defaultField  {
					Field.name = "ctime",
					Field.semantics = "Time of file creation.",
					Field.type_ = DataType "add67dbc-2e18-4ffd-aea3-b1e8cb28f7d8"
				},
				defaultField {
					Field.name = "xattr",
					Field.semantics = "Extended attributes.",
					Field.type_ =
						(Application
							(Application
								(DataType "43c6cd13-33b0-4fc8-a480-668ecb24768e")
								(DataType "4f7db06c439541658a09689d3e7dd909")
							)
							(DataType "f9f2f27af0f649b4bc8946c467c3b76a")
						)
				},
				defaultField  {
					Field.name = "file",
					Field.semantics = "The actual file.",
					Field.type_ =
						(Application
							(DataType "027770dd-5134-4ee0-8cd8-faf29e962167")
							(Variable First)
						)
				}
			]
		}
	]
}

--}


t65 = dt {
	identifier = "f2c4f6dd-d939-444b-a209-fbdf2152eb54",
  author       = Just personMikael,
	name = "SchemeSymbol",
  structure = v0 $ (dt' :: Type Zero) {
	semantics = "URI scheme symbol.",
	constructors = Just [
		defaultConstructor {
			Constructor.name = "Latin",
			Constructor.fields = [
				defaultField {
					Field.name = "latin",
					Field.type_ = DataType "6716d098a58743379e54c12f249cdc0c"
				}
			]
		},
		defaultConstructor {
			Constructor.name = "Decimal",
			Constructor.fields = [
				defaultField {
					Field.name = "decimal",
					Field.type_ = DataType "ff421b2c31774c37a7336c8245a74da9"
				}
			]
		},
		defaultConstructor { Constructor.name = "Plus" },
		defaultConstructor { Constructor.name = "Minus" },
		defaultConstructor { Constructor.name = "FullStop" }
	]
}}

t66 = dt {
	identifier = "6e2f1233-f1c8-4e6b-9bb3-7c405c666234",
  author       = Just personMikael,
	name = "SchemeName",
  structure = v0 $ (dt' :: Type Zero) {
	semantics = "URI scheme.",
	constructors = Just [
		defaultConstructor {
			Constructor.name = "SchemeName",
			Constructor.fields = [
				defaultField {
					Field.name = "initial",
					Field.type_ = DataType "6716d098a58743379e54c12f249cdc0c"
				},
				defaultField {
					Field.name = "rest",
					Field.type_ =
						(Application
							(DataType "0ba85f3f10099c75d4b696d0cf944e09")
							(DataType "f2c4f6dd-d939-444b-a209-fbdf2152eb54")
						)
				}
			]
		}
	]
}}

t67 = dt {
	identifier = "a078d512-3ead-415d-8d85-7dc6dc15b475",
  author       = Just personMikael,
	name = "RootlessPath",
  structure = v0 $ (dt' :: Type Zero) {
	semantics = "Root-less path name.",
	constructors = Just [
		defaultConstructor {
			Constructor.name = "RootlessPath",
			Constructor.fields = [
				(defaultField :: Field.Field Zero)  {
					Field.name = "segments",
					Field.semantics = "Path segments.",
					Field.type_ =
						(Application
							(DataType "0ba85f3f10099c75d4b696d0cf944e09")
							(DataType "4f7db06c439541658a09689d3e7dd909")
						)
				}
			]
		}
	]
}}

t68 = dt {
	identifier = "6ffbfb86-82ad-4f6a-89d7-3e6d36c8fc7a",
  author       = Just personMikael,
	name = "AbsolutePath",
  structure = v0 $ (dt' :: Type Zero) {
	semantics = "Absolute path name.",
	constructors = Just [
		defaultConstructor {
			Constructor.name = "AbsolutePath",
			Constructor.fields = [
				(defaultField  :: Field.Field Zero)  {
					Field.name = "segments",
					Field.semantics = "Path segments.",
					Field.type_ =
						(Application
							(DataType "0ba85f3f10099c75d4b696d0cf944e09")
							(DataType "4f7db06c439541658a09689d3e7dd909")
						)
				}
			]
		}
	]
}}

t69 = dt {
	identifier = "5448c6b7-9a08-4b4e-a40d-442c4fd2e125",
  author       = Just personMikael,
	name = "Path",
  structure = v0 $ (dt' :: Type Zero) {
	semantics = "Path name.",
	constructors = Just [
		defaultConstructor {
			Constructor.name = "AbsolutePath",
			Constructor.fields = [
				(defaultField  :: Field.Field Zero) {
					Field.name = "absolute",
					Field.type_ = DataType "6ffbfb86-82ad-4f6a-89d7-3e6d36c8fc7a"
				}
			]
		},
		defaultConstructor {
			Constructor.name = "RootlessPath",
			Constructor.fields = [
				(defaultField  :: Field.Field Zero) {
					Field.name = "rootless",
					Field.type_ = DataType "a078d512-3ead-415d-8d85-7dc6dc15b475"
				}
			]
		}
	]
}}

t70 = dt {
	identifier = "8068cbda-f35e-4618-a7e7-98c67ff9bee0",
  author       = Just personMikael,
	name = "Hierarchy",
  structure = v0 $ (dt' :: Type Zero) {
	semantics = "URI hierarchy",
	constructors = Just [
		defaultConstructor {
			Constructor.name = "Authority",
			Constructor.fields = [
				defaultField {
					Field.name = "authority",
					Field.type_ = DataType "335b7633-0e72-4b64-a525-6190fb579dad"
				},
				defaultField {
					Field.name = "absolute",
					Field.type_ = DataType "6ffbfb86-82ad-4f6a-89d7-3e6d36c8fc7a"
				}
			]
		},
		defaultConstructor {
			Constructor.name = "Path",
			Constructor.fields = [
				defaultField {
					Field.name = "rootless",
					Field.type_ = DataType "a078d512-3ead-415d-8d85-7dc6dc15b475"
				}
			]
		}
	]
}}

t71 = dt {
	identifier = "e393b15b-944c-4b35-97cd-02b1be6d693b",
  author       = Just personMikael,
	name = "URI",
  structure = v0 $ (dt' :: Type Zero) {
	semantics = "Uniform Resource Identifier",
	constructors = Just [
		defaultConstructor {
			Constructor.name = "URI",
			Constructor.fields = [
				defaultField {
					Field.name = "scheme",
					Field.type_ = DataType "6e2f1233-f1c8-4e6b-9bb3-7c405c666234"
				},
				defaultField {
					Field.name = "hierarchy",
					Field.type_ = DataType "8068cbda-f35e-4618-a7e7-98c67ff9bee0"
				},
				defaultField {
					Field.name = "query",
					Field.type_ = DataType "4f7db06c439541658a09689d3e7dd909"
				},
				defaultField {
					Field.name = "fragment",
					Field.type_ = DataType "4f7db06c439541658a09689d3e7dd909"
				}
			]
		}
	]
}}

t74 = dt {
	identifier = "a9c05900-6c8d-4849-af90-2d3ad12ee3cc",
  author       = Just personMikael,
	name = "IP",
  structure = v0 $ (dt' :: Type Zero) {
	semantics = "Internet Protocol address.",
	constructors = Just [
		(defaultConstructor :: Constructor.Constructor Zero) {
			Constructor.name = "IPv6",
			Constructor.semantics = "IP version 6 address.",
			Constructor.fields = [
				defaultField {
					Field.name = "ipv6",
					Field.type_ = DataType "bbabbac1510d49aa9da25d8033147c54"
				}
			]
		},
		(defaultConstructor :: Constructor.Constructor Zero)  {
			Constructor.name = "IPv4",
			Constructor.semantics = "IP version 4 address.",
			Constructor.fields = [
				defaultField {
					Field.name = "ipv4",
					Field.type_ = DataType "1a55145e5bd21e8adc14067707192552"
				}
			]
		}
	]
}}

t75 = dt {
	identifier = "335b7633-0e72-4b64-a525-6190fb579dad",
  author       = Just personMikael,
	name = "Authority",
  structure = v0 $ (dt' :: Type Zero) {
	constructors = Just [
		defaultConstructor {
			Constructor.name = "Authority",
			Constructor.fields = [
				(defaultField :: Field.Field Zero)  {
					Field.name = "userinfo",
					Field.semantics = "User information.",
					Field.type_ = DataType "4f7db06c439541658a09689d3e7dd909"
				},
				(defaultField :: Field.Field Zero) {
					Field.name = "host",
					Field.semantics = "Host",
					Field.type_ = DataType "9f64aa56-7f1d-4456-b7ce-f6bf7f299c06"
				},
				(defaultField :: Field.Field Zero)  {
					Field.name = "port",
					Field.semantics = "Port number",
					Field.type_ =
						(Application
							(DataType "f8f49ef6bbe874a42926fa23d5b3bc19")
							(DataType "62d2d5371f08461aa328bc06561594f6")
						)
				}
			]
		}
	]
}}

t76 = dt {
	identifier = "9f64aa56-7f1d-4456-b7ce-f6bf7f299c06",
  author       = Just personMikael,
	name = "Host",
  structure = v0 $ (dt' :: Type Zero) {
	semantics = "URI host",
	constructors = Just [
		defaultConstructor {
			Constructor.name = "IP",
			Constructor.fields = [
				(defaultField :: Field.Field Zero)  {
					Field.name = "ip",
					Field.semantics = "IP address",
					Field.type_ = DataType "a9c05900-6c8d-4849-af90-2d3ad12ee3cc"
				}
			]
		},
		defaultConstructor {
			Constructor.name = "RegName",
			Constructor.fields = [
				(defaultField  :: Field.Field Zero) {
					Field.name = "regName",
					Field.semantics = "Registered name",
					Field.type_ = DataType "4f7db06c439541658a09689d3e7dd909"
				}
			]
		}
	]
}}


