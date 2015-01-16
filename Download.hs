{-# OPTIONS
 
 -XMultiParamTypeClasses
 -XFunctionalDependencies
 -XFlexibleInstances
 -XRank2Types
 -XGADTs
 -XPolyKinds
#-}

module Main where
import System.Environment   
import System.Directory  
import System.IO  
import Control.Monad
import Data.Graph.Inductive
import qualified Data.List.Ordered
import Data.Tree
import qualified Data.List as L
import qualified Data.Map.Strict as M
import Text.ParserCombinators.Parsec
import Data.Char
import qualified Data.MultiMap as MM
import Data.Maybe

import Utilities
import IOUtilities

