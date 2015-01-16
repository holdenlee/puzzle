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

letters = ['a','b','c','d','e','f','g','h','i','j','k','l','m','n','o','p','q','r','s','t','u','v','w','x','y','z'] --A to Z

getFreq c str = 
    case str of 
      d:ss -> 
          (if d == c then 1 else 0) + getFreq c ss
      _ -> 0
                  

letterFreq :: String -> M.Map Char Int
letterFreq str = M.fromList $ map (\x -> (x,getFreq x $ map toLower str)) $ letters

freqsToCSV :: M.Map Char Int -> String
freqsToCSV m = L.intercalate "," $ map (\(x,y) -> show y) (M.toList m)

wordsToCSV :: String -> String 
wordsToCSV str = " ," ++ (L.intersperse ',' letters) ++ "\n" ++ (concat $ map (\s -> s ++ "," ++ freqsToCSV (letterFreq s) ++ "\n") (lines str))

main:: IO ()
main = do 
  args <- getArgs
  ioFileArgs args wordsToCSV
  
