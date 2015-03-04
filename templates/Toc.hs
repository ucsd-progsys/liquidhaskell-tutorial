#!/usr/bin/env runhaskell

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

module Toc where

import Data.Monoid (mempty)
import Debug.Trace
import Text.Printf (printf)
import System.Directory
import System.FilePath ((</>), takeExtension)
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

-- Use as:
-- toc templates/pagemeta.template src/ dist/page.template dist/links.txt

main :: IO ()
main = do
  (templF : tocD : outF : linkF : _ ) <- getArgs
  tplt   <- TIO.readFile templF
  toc    <- makeToc tocD
  let out = plug tplt (T.pack $ tocHtml toc)
  writeFile outF out
  writeFile linkF (tocDB toc)


plug :: T.Text -> T.Text -> String
plug tplt toc = L.unpack $ substitute tplt ctx
  where
    ctx       :: T.Text -> T.Text
    ctx "toc" = toc
    ctx z     = z


-------------------------------------------------------------------------------
type TOC  = M.Map Ref Info
type Ref  = String
data Info = Info { i_file  :: FilePath
                 , i_level :: Int
                 , i_name  :: String
                 }
            deriving (Show)

makeToc :: FilePath -> IO TOC 
makeToc dir = do
  files  <- dirLhs dir
  refs   <- mapM fileTOC [dir </> f | f <- files]
  return  $ M.fromList $ concat refs

dirLhs dir = do
  fs <- getDirectoryContents dir
  let fs' = [f | f <- fs, takeExtension f == ".lhs"]
  return $ trace ("dirLhs: in = " ++ show fs ++ "out = " ++ show fs') fs'

readDoc   :: FilePath -> IO Pandoc
readDoc f = do str <- readFile f
               let doc = readMarkdown def str --  <$> readFile f
               return $ trace ("DOC=" ++ show doc) doc

fileTOC :: FilePath -> IO [(Ref, Info)]
fileTOC f = query (getRef f) <$> readDoc f

-- genToc :: [(FilePath, Pandoc)] -> TOC
-- genToc fds = M.fromList $ concat $ [ qq f d | (f, d) <- fds']
--   where
--     foo f  = trace ("genToc: FILE = " ++ f) f
--     qq f d = query (getRef $ foo f) d
--     fds'   = trace ("JUNK" ++ show fds) fds

getRef :: FilePath -> Block -> [(Ref, Info)]
getRef f b@(Header n (l,_,_) is) = {- trace ("MHEADER: " ++ show b) $ -}
                                   [(l, Info f n $ inlineString is)]
getRef _ _                       = []

tocHtml     :: TOC -> String
tocHtml toc = "<!-- undefined" ++  show toc ++ "-->"

tocDB :: TOC -> String
tocDB = undefined

inlineString    = writeHtmlString def . inlinePandoc
inlinePandoc is = Pandoc mempty [Plain is]

