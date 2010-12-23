{-# LANGUAGE OverloadedStrings #-}
module TypeableInternal.Classes where

import Prelude hiding (maybe)

--import Typeable.Cc6ebaa9f4cdc4068894d1ffaef5a7a83
import Typeable.T421496848904471ea3197f25e2a02b72
--import Typeable.T9e2e1e478e094a8abe5507f8574ac91f

import TypeableInternal.TypesDefault
import TypeableInternal.InternalTypeDefs
import qualified Data.Set as S
import qualified Data.Map as M

-----
-- class-definitions (provisoric for as long as the binary format is not yet finalised)
-----

c1  = defaultClass
      { classIdentifier   = "8658bc79-8a87-4218-9aa7-c70e2f9d0fe2"
      , className         = "Eq"
      , classSemantics    = "An equivalence relation defined on $a. The default instance should indicate syntactic identity. All instances must obey reflexivity, symmetrie and transitivity."
      , classMethods      =            [ Method
                                           "equal" 
                                           (function (Variable First) (function (Variable First) (DataType "0219c59f732a8ef507215fbdb4cceacd"))) 
                                           ""
                                       , Method 
                                           "unequal" 
                                           (function (Variable First) (function (Variable First) (DataType "0219c59f732a8ef507215fbdb4cceacd"))) 
                                           "Should be true iff equal is false."
                                       ]
      }

c2  = defaultClass
      { classIdentifier   = "2980bc10-f5f2-4605-8ab3-5dbaaa4e1663"
      , className         = "Kind"
      , classSemantics    = "A type's kind."
      }


c3  = defaultClass
      { classIdentifier   = "96d7607d-b4b8-4dc2-9a37-313009d5924b"
      , className         = "Ord"
      , classSemantics    = "A complete order relation defined on $a. All instances must obey reflexivity, antisymmetrie, transitivity."
      , classMethods      =            [ Method
                                           "compare" 
                                           (function (Variable First) (function (Variable First) (DataType "f4b6d72c-609d-4003-ba98-917f8c56a678"))) 
                                           ""
                                       , Method
                                           "less" 
                                           (function (Variable First) (function (Variable First) (DataType "0219c59f732a8ef507215fbdb4cceacd"))) 
                                           "$a is strictly less than $b."
                                       , Method 
                                           "lessEq" 
                                           (function (Variable First) (function (Variable First) (DataType "0219c59f732a8ef507215fbdb4cceacd"))) 
                                           "$a is less or equal than $b."
                                       , Method 
                                           "greater" 
                                           (function (Variable First) (function (Variable First) (DataType "0219c59f732a8ef507215fbdb4cceacd"))) 
                                           "$a is strictly greater thatn $b."
                                       , Method 
                                           "greaterEq" 
                                           (function (Variable First) (function (Variable First) (DataType "0219c59f732a8ef507215fbdb4cceacd"))) 
                                           "$a is greater or equal than $b."
                                       , Method 
                                           "min" 
                                           (function (Variable First) (function (Variable First) (Variable First))) 
                                           "Returns the least of both elements."
                                       , Method 
                                           "max" 
                                           (function (Variable First) (function (Variable First) (Variable First))) 
                                           "Returns the greatest of both elements."
                                       ]
      }

c4  = defaultClass
      { classIdentifier   = "34375052-1533-45b0-9a13-49a77ea57ee1"
      , className         = "Enum"
      , classSemantics    = "Instances of this class are enumeratable. Enumeration is possible in positive as well as negative direction."
      , classMethods      =            [ Method
                                           "succ" 
                                           (function (Variable First) (Variable First)) 
                                           "The succeeding element. It must hold that $a < (succ $a)."
                                       , Method 
                                           "pred" 
                                           (function (Variable First) (Variable First)) 
                                           "The preceding element. It must hold that $a == pred (succ $a)."
                                       , Method 
                                           "toEnum" 
                                           (function (DataType "ac2e770f2132aced749ec197385ff552") (Variable First)) 
                                           ""
                                       , Method
                                           "fromEnum" 
                                           (function (Variable First) (DataType "ac2e770f2132aced749ec197385ff552")) 
                                           ""
                                       , Method
                                           "enumFrom" 
                                           (function (Variable First) (function (Variable First) (list $ Variable First))) 
                                           ""
                                       , Method
                                           "enumFromThen" 
                                           (function (Variable First) (function (Variable First) (list $ Variable First))) 
                                           ""
                                       , Method
                                           "enumFromTo" 
                                           (function (Variable First) (function (Variable First) (list $ Variable First))) 
                                           ""
                                       , Method 
                                           "enumFromThenTo" 
                                           (function (Variable First) (function (Variable First) (function (Variable First) (list $ Variable First)))) 
                                           ""
                                       ]
      }

c5  = defaultClass
      { classIdentifier   = "d4bb0916-fb93-4233-b445-8ffcc69e5bd3"
      , className         = "Bounded"
      , classSemantics    = "Instances of this class have a lower and an upper bound."
      , classMethods      =            [ Method
                                           "minBound" 
                                           (Variable First)  
                                           "The lower bound."
                                       , Method
                                           "maxBound" 
                                           (Variable First)  
                                           "The upper bound."
                                       ]
      }

c6  = defaultClass
      { classIdentifier   = "c6ebaa9f4cdc4068894d1ffaef5a7a83"
      , className         = "PeanoNumber"
      , classSemantics    = "Typelevel natural numbers. The only instance of this type should be ->Zero and ->Succ."
      , classMethods      = [] 
      }

c7  = defaultClass
      { classIdentifier   = "882f4a6affa24579830e0a850acad145"
      , className         = "TimeStandard"
      , classSemantics    = ""
      , classMethods      = []
      }

c8  = defaultClass
      { classIdentifier   = "584d85dcf5a144bebf0da92b7a5977a3"
      , className         = "Functor"
      , classSemantics    = ""
      , classMethods      =            [ Method
                                           "fmap" 
                                           (Forall $
                                             Forall $   
                                               function
                                                 (function (Variable (Next First)) (Variable (Next (Next First))))
                                                 (function
                                                   (Application (Variable First) (Variable (Next First)))
                                                   (Application (Variable First) (Variable (Next (Next First))))
                                                 )
                                           )
                                           "Takes a function from $b to $c and returns a functor."
                                       ]

      }


c9  = defaultClass
      { classIdentifier   = "30c5342d3f7243d29b04c5f9abb72405"
      , className         = "Applicative"
      , classSemantics    = ""
      , classConstraints  = S.fromList [Constraint  "584d85dcf5a144bebf0da92b7a5977a3" [Variable First]]
      , classMethods      = [ Method
                                "pure"
                                (Forall $
                                  function (Variable (Next First)) (Application (Variable First) (Variable (Next First)))
                                )
                                ""
                            , Method
                                "sequence"
                                (Forall $ Forall $
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
      }

c10  = defaultClass
      { classIdentifier   = "d1c2d3e54e6f4910b9c83bd3c35617c6"
      , className         = "Monad"
      , classSemantics    = ""
      , classMethods      = [ Method
                                "bind"
                                (Forall $ Forall $
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
                            , Method
                                "bind'"
                                (Forall $ Forall $
                                  function
                                    (Application (Variable First) (Variable (Next First)))
                                    (function 
                                      (Application (Variable First) (Variable (Next (Next First))))
                                      (Application (Variable First) (Variable (Next (Next First))))
                                    )
                                )
                                ""
                            , Method
                                "return"
                                (Forall $ 
                                  function
                                    (Variable (Next First))
                                    (Application (Variable First) (Variable (Next First)))
                                )
                                ""
                            , Method
                                "fail"
                                (Forall $ 
                                  function
                                    (list (DataType "16f4245df3cc0b534f028235ff8aae16"))
                                    (Application (Variable First) (Variable (Next First)))
                                )
                                ""
                            ] 
      }

c11  = defaultClass
      { classIdentifier   = "8cbc0d8529974ffc838ea3225176feb4"
      , className         = "Show"
      , classSemantics    = ""
      , classMethods      = [ Method
                                "show"
                                (function
                                  (Variable First) 
                                  (list (DataType "16f4245df3cc0b534f028235ff8aae16"))
                                )
                                ""
                            ] 
      }

c12  = defaultClass
      { classIdentifier   = "56b4f8a2d8b34e138a8349ad7e3de441"
      , className         = "Read"
      , classSemantics    = ""
      , classMethods      = [ Method
                                "read"
                                (function
                                  (list (DataType "16f4245df3cc0b534f028235ff8aae16"))
                                  (Variable First) 
                                )
                                ""
                            ] 
      }


