#!/usr/bin/env runhaskell

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
{-@ LIQUID "--diff" @-}

module Main (main) where

import Data.Monoid (mempty)
import Debug.Trace
import Text.Printf (printf)
import System.Directory
import System.FilePath ((</>), replaceExtension, takeFileName, takeExtension)
import System.IO
import qualified Data.ByteString.Lazy as S
import Text.Pandoc
import Text.Pandoc.Walk (query)
import Control.Monad
import Control.Applicative ((<$>))

import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.Lazy as L
import qualified Data.Text.IO as TIO
import System.Environment (getArgs)
import Data.Text.Template
import qualified Data.Text.IO as TIO

-- Use as:
-- toc src/ templates/pagemeta.template templates/index.template dist/page.template src/index.html dist/links.txt

main :: IO ()
main = do
  [tocD,pageT,indexT,pageF,indexF,linkF] <- getArgs
  tplt    <- TIO.readFile pageT
  itplt   <- TIO.readFile indexT
  toc     <- makeToc tocD
  let tocT = T.pack $ tocHtml toc
  writeFile pageF  $ plug tplt  tocT
  writeFile indexF $ plug itplt tocT
  writeFile linkF  $ tocDB toc

plug :: T.Text -> T.Text -> String
plug tplt toc = L.unpack $ substitute tplt ctx
  where
    ctx       :: T.Text -> T.Text
    ctx "toc" = toc
    ctx z     = z

-------------------------------------------------------------------------------

newtype TOC = TOC [(Int, FilePath, [(Ref, Info)])]
              deriving (Show)

type Ref  = String

data Info = Info { i_file  :: FilePath
                 , i_level :: Int
                 , i_name  :: String
                 }
            deriving (Show)

data Chapter = Ch { c_num  :: Int
                  , c_name :: (Ref, Info)
                  , c_secs :: [(Int, Ref, Info)]
                  }
               deriving (Show)

-------------------------------------------------------------------------------

makeToc :: FilePath -> IO TOC
makeToc dir = do
  files  <- dirLhs dir
  refs   <- mapM fileTOC [dir </> f | f <- files]
  return  $ TOC $ zip3 [1..] files refs -- M.fromList $ concat refs

dirLhs dir = do
  fs <- getDirectoryContents dir
  let fs' = [f | f <- fs, takeExtension f == ".lhs"]
  return fs'

readDoc   :: FilePath -> IO Pandoc
readDoc f = do
  str <- TIO.readFile f
  let md =  runPure $ readMarkdown def str
  case md of
    Right d -> return d
    -- Left e  -> error $ "readDoc hits Error!: " ++ show e



fileTOC :: FilePath -> IO [(Ref, Info)]
fileTOC f = query (getRef f) <$> readDoc f


getRef :: FilePath -> Block -> [(Ref, Info)]
getRef f b@(Header n (l,_,_) is) = [(l, Info f n $ inlineString is)]
getRef _ _                       = []

tocHtml     :: TOC -> String
tocHtml toc = unlines $  ["<ul class='chapter'>"]
                      ++ chaptersHtml (tocChapters toc)
                      ++ ["</ul>"]

tocChapters :: TOC -> [Chapter]
tocChapters (TOC zs) = map mkChapter zs

mkChapter :: (Int, FilePath, [(Ref, Info)]) -> Chapter
mkChapter (i, f, ri:ris) = Ch i ri secs
  where
    secs = zipWith (\j (x, y) -> (j,x,y)) [1..] ris


chaptersHtml :: [Chapter] -> [String]
chaptersHtml = concatMap chapterHtml

chapterHtml    :: Chapter -> [String]
chapterHtml ch =  [secHtml ch]
               ++ ["<ul class='section'>"]
               ++ (subsecHtml (c_num ch) <$> c_secs ch)
               ++ ["</ul>"]

secHtml :: Chapter -> String
secHtml ch = printf "<li><a href='%s'><b>%d.</b>%s</a></li>" cFile cNum cName
  where
    (_,ci) = c_name ch
    cNum   = c_num ch
    cName  = i_name ci
    cFile  = htmlFile (i_file ci)

subsecHtml :: Int -> (Int, Ref, Info) -> String
subsecHtml cNum (sNum, r, i)
  = printf "<li><a href='%s'><b>%d.%d.</b> %s</a></li>"
      sLink cNum sNum sName
  where
    sLink = makeLink (i_file i) r
    sName = i_name i

makeLink file ref = htmlFile file ++ "#" ++ ref

htmlFile file = (takeFileName file) `replaceExtension` ".html"

-------------------------------------------------------------------------------

inlineString = either errHandler T.unpack . runPure . writeHtml5String def . inlinePandoc
  where errHandler = error $ mconcat ["inlineString"]
inlinePandoc is = Pandoc mempty [Plain is]

-------------------------------------------------------------------------------

tocDB   :: TOC -> String
tocDB _ = "undefined"
