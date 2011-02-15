module Main where

import Typeable.T421496848904471ea3197f25e2a02b72

import Control.Monad
import Data.Monoid
import qualified Data.Set as S
import qualified Data.Map as M
import Data.String

import TypeableInternal.InternalTypeDefs
import TypeableInternal.Types
import TypeableInternal.Classes
import TypeableInternal.TypesDefault
import TypeableInternal.Context
import TypeableInternal.FormatHaskell
import System.IO.Unsafe
import System.FilePath.Posix

import Language.Haskell.Exts.Syntax (Module)
import Language.Haskell.Exts.Pretty

main :: IO ()
main  = print "hallo" 

-----------------

static = Static (g types) (g classes)
         where
           g z = M.fromList (map (\x->(identifier x, x)) z)

runC x = runContext x static
        
