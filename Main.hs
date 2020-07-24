module Main where 

import qualified Data.List as L 
import System.Directory (listDirectory)
import System.FilePath (addExtension)
import System.FilePath ((</>))

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