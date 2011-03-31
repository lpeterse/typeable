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
                  largeword,
                  mtl,
                  haskell98,
                  haskell-src-exts,
                  time, time-extras,
                  process,
                  containers,
                  bytestring,
                  binary,
                  filepath,
                  text,
                  blaze-html,
                  happstack-server >= 0.5.0.4, 
                  old-locale,     
                  parsec >= 3.0.0
 hs-source-dirs:  src
 Exposed-Modules: Data.EBF
                  Typeable.Internal.FormatHtml, 
                  Typeable.Internal.Types, 
                  Typeable.Internal.Classes, 
                  Typeable.Internal.TypesDefault, 
                  Typeable.Internal.InternalTypeDefs, 
                  Typeable.Internal.FormatHaskell, 
                  Typeable.Internal.Context, 
                  Typeable.Internal.NamespaceParser,
                  Typeable.Internal.Graph
