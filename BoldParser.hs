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

--import qualified Text.Parsec.Token as P
--import Text.Parsec.Language (emptyDef)
--import qualified Text.Parsec.Prim as Pr

-- http://stackoverflow.com/questions/5034685/what-is-the-haskell-syntax-to-import-modules-in-subdirectories
-- All you need to do is pass the -i flag to the compiler with a colon-delimited list of directories. The compiler will then check those directories for the source files of the imported modules.
import Utilities
import IOUtilities

getBolds = getWithBeginEnd "<b>" "</b>"

getWithBeginEnd:: String -> String -> [String] -> Parser [String]
getWithBeginEnd x y strs = do
  (try $ do
    string x
    boldLetters <- manyTill anyChar (string y)
    getBolds (strs ++ [boldLetters])) <|>
   (do
    anyChar
    getBolds strs) <|>
   (do
    eof
    return strs)

parseBold:: String -> String
parseBold s = case (parse (getBolds []) "" s) of 
                Left err -> show err
                Right strs -> L.intercalate "\n" strs

main:: IO ()
main = do 
  args <- getArgs
  ioFileArgs args parseBold

--  let a0 = if (length args == 0) then "in.txt" else (args!!0)
--  let a1 = if (length args <= 1) then "out.txt" else (args!!1)
--  ioFile a0 a1 parseBold
