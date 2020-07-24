module Main where 

import qualified Data.List as L 
import System.Directory (listDirectory)

main :: IO ()
main = hideIgnores -- return ()

hideIgnores :: IO ()
hideIgnores = do
  files <- listDirectory "src/"
  mapM hideIgnore files
  return ()

hideIgnore :: FilePath -> IO ()
hideIgnore f = do
    str <- unlines . map hide . lines <$> readFile f
    writeFile f str

hide :: String -> String
hide str 
  | L.isPrefixOf "{-@ ignore " str = "-- " ++ str
  | L.isPrefixOf "{-@ fail "   str = "-- " ++ str
  | otherwise                      = str