module Main where 

import qualified Data.List as L 
import System.Directory (listDirectory)
import System.FilePath (addExtension)
import System.FilePath ((</>))

import Data.Char (toUpper)
import System.Directory (renameFile, getTemporaryDirectory)
import System.Environment (getArgs)

-- main = do
--     [file] <- getArgs
--     tmpDir <- getTemporaryDirectory
--     let tmpFile = tmpDir ++ "/" ++ file
--     readFile file >>= writeFile tmpFile . map toUpper
--     renameFile tmpFile file

main :: IO ()
main = hideIgnores -- return ()

hideIgnores :: IO ()
hideIgnores = do
  files <- filter (L.isSuffixOf ".lhs") <$> listDirectory "src/"
  mapM hideIgnore files
  return ()

hideIgnore :: FilePath -> IO ()
hideIgnore file = do
  str <- unlines . map hide . lines <$> readFile ("src" </> file)
  let tmpFile = "src/out" </> file
  writeFile tmpFile str

hide :: String -> String
hide str 
  | L.isPrefixOf "{-@ ignore " str = "-- " ++ str
  | L.isPrefixOf "{-@ fail "   str = "-- " ++ str
  | otherwise                      = str