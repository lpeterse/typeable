Name:            typeable
Version:         0.0
Cabal-Version:   >= 1.2
Build-Type:      Simple
License:         BSD3
License-File:    LICENSE
Copyright:       (c) 2010 OpenCorpus Research Group - University of Osbnabrueck
Author:          OpenCorpus Research Group <info@open-corpus.org>
Maintainer:      OpenCorpus Research Group <info@open-corpus.org>
Bug-Reports:     http://trac.open-corpus.org/
Stability:       alpha
Homepage:        http://typeable.org
Category:        
Tested-With:     GHC == 6.12.3
Synopsis:        Web server for collecting and serving type definitions.
Description:     follows up
                 .
                 please be patient 
Data-Files:
                 README, INSTALL, BUGS, changelog

Executable        typeserver 
 hs-source-dirs:  src
 main-is:         typeserver.hs
 Build-depends:   base >= 4.0
 ghc-options:     -threaded

Library
 Build-Depends:   base >= 4.0,
                  Crypto >= 4.2.2,
                  mtl,
                  haskell98,
                  haskell-src-exts,
                  time, time-extras,
                  process,
                  containers,
                  bytestring,
                  filepath,
                  text,
                  blaze-html,
                  url,
                  happstack-server >= 0.5.0.4, 
                  old-locale,     
                  parsec >= 3.0.0
 hs-source-dirs:  src
 Exposed-Modules: TypeableInternal.FormatHtml, 
                  TypeableInternal.Types, 
                  TypeableInternal.Classes, 
                  TypeableInternal.TypesDefault, 
                  TypeableInternal.InternalTypeDefs, 
                  TypeableInternal.FormatHaskell, 
                  TypeableInternal.Context, 
                  TypeableInternal.NamespaceParser,
                  Typeable.Cc6ebaa9f4cdc4068894d1ffaef5a7a83,
                  Typeable.T421496848904471ea3197f25e2a02b72,
                  Typeable.T9e2e1e478e094a8abe5507f8574ac91f,
                  Typeable.Cb5ba7ec44dbb4236826c6ef6bc4837e4, 
                  Typeable.Taf20e1db8f0d414f90625b1521e41378,
                  -- UTC
                  Typeable.Tc1b1f6c722c2436fab3180146520814e,
                  -- Time
                  Typeable.T606f253533d3420da3465afae341d598
