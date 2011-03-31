{-# LANGUAGE OverloadedStrings #-}
module Typeable.Internal.Classes where

import Typeable.T9e2e1e478e094a8abe5507f8574ac91f --Succ
import Typeable.T421496848904471ea3197f25e2a02b72 --Zero
import Typeable.T606f253533d3420da3465afae341d598 --Time
import Typeable.T0174bd2264004820bfe34e211cb35a7d hiding (function) --DataType
import Typeable.T2a94a7a8d4e049759d8dd546e72293ff --Constraint
import qualified Typeable.Te590e9ce9cea4dfe86a413e9270dd1c2 as Method --Method
import qualified Typeable.T4e0b8f8ea2b145228fa4ec74b559bf6a as Class --Class
import Typeable.T451f847e1cb642d0b7c5dbdfa03f41b5 --Definition

import Prelude hiding (maybe)

import Typeable.Internal.TypesDefault
import Typeable.Internal.InternalTypeDefs
import qualified Data.Set as S
import qualified Data.Map as M

-----
-- class-definitions (provisoric for as long as the binary format is not yet finalised)
--

classes :: [Definition Class.Class]
classes = [
            c1_  -- Eq
          , c2  -- Kind
          , c3  -- Ord
          , c4  -- Enum
          , c5  -- Bounded
          , c6  -- PeanoNumber
          , c7  -- TimeStandard
          , c8  -- Functor
          , c9  -- Applicative
          , c10 -- Monad
          , c11 -- Read
          , c12 -- Show
          ]

dc :: Definition Class.Class
dc  = Definition 
        { identifier       = undefined
        , antecedent       = Nothing
        , creationTime     = Time 3499718400
        , modificationTime = Time 3499718400
        , author           = Nothing
        , maintainer       = defaultPerson
        , name             = undefined
        , structure        = c1 dc'  
        }

dc' :: Class.Class (Succ Zero)
dc' = Class.Class { Class.semantics = "", Class.constraints = S.empty, Class.methods = [] }

----

c1_  = dc
      { identifier   = "8658bc79-8a87-4218-9aa7-c70e2f9d0fe2"
      , name         = "Eq"
      , structure    = c1 $ dc'
      { Class.semantics    = "An equivalence relation defined on $a. The default instance should indicate syntactic identity. All instances must obey reflexivity, symmetrie and transitivity."
      , Class.methods      =            [ Method.Method
                                           "equal" 
                                           (function (Variable First) (function (Variable First) (DataType "0219c59f732a8ef507215fbdb4cceacd"))) 
                                           ""
                                       , Method.Method 
                                           "unequal" 
                                           (function (Variable First) (function (Variable First) (DataType "0219c59f732a8ef507215fbdb4cceacd"))) 
                                           "Should be true iff equal is false."
                                       ]
      }}

c2  = dc
      { identifier   = "2980bc10-f5f2-4605-8ab3-5dbaaa4e1663"
      , name         = "Kind"
      , structure    = c1 $ dc'
      { Class.semantics    = "A type's kind."
      }}


c3  = dc
      { identifier   = "96d7607d-b4b8-4dc2-9a37-313009d5924b"
      , name         = "Ord"
      , structure    = c1 $ dc'
      { Class.semantics    = "A complete order relation defined on $a. All instances must obey reflexivity, antisymmetrie, transitivity."
      , Class.methods      =            [ Method.Method
                                           "compare" 
                                           (function (Variable First) (function (Variable First) (DataType "f4b6d72c-609d-4003-ba98-917f8c56a678"))) 
                                           ""
                                       , Method.Method
                                           "less" 
                                           (function (Variable First) (function (Variable First) (DataType "0219c59f732a8ef507215fbdb4cceacd"))) 
                                           "$a is strictly less than $b."
                                       , Method.Method 
                                           "lessEq" 
                                           (function (Variable First) (function (Variable First) (DataType "0219c59f732a8ef507215fbdb4cceacd"))) 
                                           "$a is less or equal than $b."
                                       , Method.Method 
                                           "greater" 
                                           (function (Variable First) (function (Variable First) (DataType "0219c59f732a8ef507215fbdb4cceacd"))) 
                                           "$a is strictly greater thatn $b."
                                       , Method.Method 
                                           "greaterEq" 
                                           (function (Variable First) (function (Variable First) (DataType "0219c59f732a8ef507215fbdb4cceacd"))) 
                                           "$a is greater or equal than $b."
                                       , Method.Method 
                                           "min" 
                                           (function (Variable First) (function (Variable First) (Variable First))) 
                                           "Returns the least of both elements."
                                       , Method.Method 
                                           "max" 
                                           (function (Variable First) (function (Variable First) (Variable First))) 
                                           "Returns the greatest of both elements."
                                       ]
      }}

c4  = dc
      { identifier   = "34375052-1533-45b0-9a13-49a77ea57ee1"
      , name         = "Enum"
      , structure    = c1 $ dc'
      { Class.semantics    = "Instances of this class are enumeratable. Enumeration is possible in positive as well as negative direction."
      , Class.methods      =            [ Method.Method
                                           "succ" 
                                           (function (Variable First) (Variable First)) 
                                           "The succeeding element. It must hold that $a < (succ $a)."
                                       , Method.Method 
                                           "pred" 
                                           (function (Variable First) (Variable First)) 
                                           "The preceding element. It must hold that $a == pred (succ $a)."
                                       , Method.Method 
                                           "toEnum" 
                                           (function (DataType "ac2e770f2132aced749ec197385ff552") (Variable First)) 
                                           ""
                                       , Method.Method
                                           "fromEnum" 
                                           (function (Variable First) (DataType "ac2e770f2132aced749ec197385ff552")) 
                                           ""
                                       , Method.Method
                                           "enumFrom" 
                                           (function (Variable First) (function (Variable First) (list $ Variable First))) 
                                           ""
                                       , Method.Method
                                           "enumFromThen" 
                                           (function (Variable First) (function (Variable First) (list $ Variable First))) 
                                           ""
                                       , Method.Method
                                           "enumFromTo" 
                                           (function (Variable First) (function (Variable First) (list $ Variable First))) 
                                           ""
                                       , Method.Method 
                                           "enumFromThenTo" 
                                           (function (Variable First) (function (Variable First) (function (Variable First) (list $ Variable First)))) 
                                           ""
                                       ]
      }}

c5  = dc
      { identifier   = "d4bb0916-fb93-4233-b445-8ffcc69e5bd3"
      , name         = "Bounded"
      , structure    = c1 $ dc'
      { Class.semantics    = "Instances of this class have a lower and an upper bound."
      , Class.methods      =            [ Method.Method
                                           "minBound" 
                                           (Variable First)  
                                           "The lower bound."
                                       , Method.Method
                                           "maxBound" 
                                           (Variable First)  
                                           "The upper bound."
                                       ]
      }}

c6  = dc
      { identifier   = "c6ebaa9f4cdc4068894d1ffaef5a7a83"
      , name         = "PeanoNumber"
      , structure    = c1 $ dc'
      { Class.semantics    = "Typelevel natural numbers. The only instance of this type should be ->Zero and ->Succ."
      , Class.methods      = [] 
      }}

c7  = dc
      { identifier   = "882f4a6affa24579830e0a850acad145"
      , name         = "TimeStandard"
      , structure    = c1 $ dc'
      { Class.semantics    = ""
      , Class.methods      = []
      }}

c8  = dc
      { identifier   = "584d85dcf5a144bebf0da92b7a5977a3"
      , name         = "Functor"
      , structure    = c1 $ dc'
      { Class.semantics    = ""
      , Class.methods      =            [ Method.Method
                                           "fmap" 
                                           (Forall S.empty $
                                             Forall S.empty $   
                                               function
                                                 (function (Variable (Next First)) (Variable (Next (Next First))))
                                                 (function
                                                   (Application (Variable First) (Variable (Next First)))
                                                   (Application (Variable First) (Variable (Next (Next First))))
                                                 )
                                           )
                                           "Takes a function from $b to $c and returns a functor."
                                       ]

      }}


c9  = dc
      { identifier   = "30c5342d3f7243d29b04c5f9abb72405"
      , name         = "Applicative"
      , structure    = c1 $ dc'
      { Class.semantics    = ""
      , Class.constraints  = S.fromList [Constraint  "584d85dcf5a144bebf0da92b7a5977a3" (Variable First)]
      , Class.methods      = [ Method.Method
                                "pure"
                                (Forall S.empty $
                                  function (Variable (Next First)) (Application (Variable First) (Variable (Next First)))
                                )
                                ""
                            , Method.Method
                                "sequence"
                                (Forall S.empty $ Forall S.empty $
                                  function 
                                    (Application
                                      (Variable First)
                                      (function (Variable (Next First)) (Variable (Next (Next First))))
                                    )
                                    (function
                                      (Application (Variable First) (Variable (Next First)))
                                      (Application (Variable First) (Variable (Next (Next First))))
                                    )
                                )
                                "<*>"
                            ] 
      }}

c10  = dc
      { identifier   = "d1c2d3e54e6f4910b9c83bd3c35617c6"
      , name         = "Monad"
      , structure    = c1 $ dc'
      { Class.semantics    = ""
      , Class.methods      = [ Method.Method
                                "bind"
                                (Forall S.empty $ Forall S.empty $
                                  function
                                    (Application (Variable First) (Variable (Next First)))
                                    (function 
                                      (function
                                        (Variable (Next First)) 
                                        (Application (Variable First) (Variable (Next (Next First))))
                                      )
                                      (Application (Variable First) (Variable (Next (Next First))))
                                    )
                                )
                                ""
                            , Method.Method
                                "bind'"
                                (Forall S.empty $ Forall S.empty $
                                  function
                                    (Application (Variable First) (Variable (Next First)))
                                    (function 
                                      (Application (Variable First) (Variable (Next (Next First))))
                                      (Application (Variable First) (Variable (Next (Next First))))
                                    )
                                )
                                ""
                            , Method.Method
                                "return"
                                (Forall S.empty $ 
                                  function
                                    (Variable (Next First))
                                    (Application (Variable First) (Variable (Next First)))
                                )
                                ""
                            , Method.Method
                                "fail"
                                (Forall S.empty $ 
                                  function
                                    (list (DataType "16f4245df3cc0b534f028235ff8aae16"))
                                    (Application (Variable First) (Variable (Next First)))
                                )
                                ""
                            ] 
      }}

c11  = dc
      { identifier   = "8cbc0d8529974ffc838ea3225176feb4"
      , name         = "Show"
      , structure    = c1 $ dc'
      { Class.semantics    = ""
      , Class.methods      = [ Method.Method
                                "show"
                                (function
                                  (Variable First) 
                                  (list (DataType "16f4245df3cc0b534f028235ff8aae16"))
                                )
                                ""
                            ] 
      }}

c12  = dc
      { identifier   = "56b4f8a2d8b34e138a8349ad7e3de441"
      , name         = "Read"
      , structure    = c1 $ dc'
      { Class.semantics    = ""
      , Class.methods      = [ Method.Method
                                "read"
                                (function
                                  (list (DataType "16f4245df3cc0b534f028235ff8aae16"))
                                  (Variable First) 
                                )
                                ""
                            ] 
      }}


