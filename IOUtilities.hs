{-# OPTIONS
 
 -XMultiParamTypeClasses
 -XFunctionalDependencies
 -XFlexibleInstances
 -XRank2Types
 -XGADTs
 -XPolyKinds
#-}

module IOUtilities where
import System.Environment   
import System.Directory  
import System.IO
import System.IO.Unsafe


--unsafe, quick and dirty
getFile::String -> String
getFile name = unsafePerformIO (readFile name)


ioFile:: String -> String -> (String -> String) -> IO ()
ioFile inputF outputF f =
 do  
  handle <- openFile inputF ReadMode
  contents <- hGetContents handle
  writeFile outputF (f contents)

ioFiles:: [String] -> String -> (String -> String) -> IO ()
ioFiles inputFs outputF f =
  do  
--illegal
    handles <- sequence (fmap (\x -> openFile x ReadMode) inputFs)
    contents <- sequence (fmap hGetContents handles)
    let content = unlines contents
    writeFile outputF (f content)

ioFileArgs :: [String] -> (String -> String) -> IO ()
ioFileArgs args f = 
    do
      let a0 = if (length args == 0) then "in.txt" else (args!!0)
      let a1 = if (length args <= 1) then "out.txt" else (args!!1)
      ioFile a0 a1 f
