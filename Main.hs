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
  tmpDir <- getTemporaryDirectory
  files <- filter (L.isSuffixOf ".lhs") <$> listDirectory "src/"
  mapM (hideIgnore tmpDir . ("src" </>)) files
  return ()

hideIgnore :: FilePath -> FilePath -> IO ()
hideIgnore tmpDir f = do
    let tmpFile = tmpDir </> f
    str <- unlines . map hide . lines <$> readFile f
    writeFile tmpFile str
    renameFile tmpFile file

hide :: String -> String
hide str 
  | L.isPrefixOf "{-@ ignore " str = "-- " ++ str
  | L.isPrefixOf "{-@ fail "   str = "-- " ++ str
  | otherwise                      = str