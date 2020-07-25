{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-@ LIQUID "--no-termination" @-}

module Figures (main) where

import Data.IORef
import Text.Pandoc.JSON
import Text.Pandoc
import Text.Pandoc.Walk (walkM)
import Data.List (isSuffixOf, isPrefixOf)
import Debug.Trace
import Text.Printf (printf)

import System.FilePath (takeFileName, takeExtension)

import System.Directory
import System.IO
import Control.Applicative ((<$>))
import Control.Monad (replicateM_)

import qualified Data.ByteString.Lazy as S
import qualified Data.Text as T
import qualified Data.Text.Lazy as L
import qualified Data.Text.IO as TIO
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import System.Environment (getEnv)

import Data.Char (isDigit)
import Data.Text.Template

main :: IO ()
main = do
  tgt <- getEnv "PANDOC_TARGET"
  let out = output tgt
  let ch  = fileChapter tgt
  txFig ch out

data Output = HTML | LATEX deriving (Eq)

instance Show Output where
  show HTML  = "html"
  show LATEX = "latex"

output :: FilePath -> Output
output = extOut . takeExtension
  where
    extOut ".html" = HTML
    extOut ".pdf"  = LATEX
    extOut s       = error $ "Figures : unknown target: " ++ s

fileChapter :: FilePath -> Maybe Int
fileChapter = toInt . takeWhile isDigit . takeFileName
  where
    toInt "" = Nothing
    toInt xs = Just $ read xs

txFig :: Maybe Int -> Output -> IO ()
txFig ch HTML  = txFigures ch HTML  "" "templates/figHtml.template"
txFig ch LATEX = txFigures ch LATEX "" "templates/figLatex.template"


txFigures :: Maybe Int -> Output -> FilePath -> FilePath -> IO ()
txFigures ch tgt prefix templateF
  = do r    <- newIORef emptyInfo
       tplt <- TIO.readFile templateF
       resetChapter tgt ch r
       toJSONFilter (tx tgt (T.pack prefix) tplt r)

resetChapter LATEX _        _ = return ()
resetChapter HTML  Nothing  _ = return ()
resetChapter HTML  (Just n) r = replicateM_ (n-1) (newChapter r)

tx tgt prefix t r b0
  = do b1 <- txBlock tgt prefix t r b0
       b2 <- txLink               r b1
       return b2

txLink r = walkM (reLink r)

reLink   :: IORef Info -> Inline -> IO Inline
-- reLink r (Link attr [Str "auto"] tgt@('#':id,_))
reLink r i@(Link attr [Str "auto"] tgt@(pid,_)) = case T.unpack pid of
  '#':id -> do n <- getCount r (T.pack id)
               return $ Link attr [Str (T.pack $ show n)] tgt
  _      -> return i

reLink _ i
  = return i

txBlock _   _      _ r z@(Header 1 _ _)
  = newChapter r >> return z

txBlock tgt prefix t r (Div (id, [cls], kvs) _)
  | isFigure cls
  = makeFigure tgt prefix t r id cls kvs

txBlock _ _ _ _ z
  = return z -- $ trace ("IAMTHIS:" ++ show z) z

isFigure s    = s `elem` ["figure", "marginfigure"]

makeFigure tgt prefix t r id cls kvs
  = RawBlock (Format tgtTxt) . pad prefix t id cls kvs <$> getCount r id
    where tgtTxt = T.pack $ show tgt 

-- pad :: Text -> _ -> _ -> _ -> [(Text, Text)]
pad prefix tplt id cls kvs n
  = {- trace ("PAD" ++ show res) $ -} res
  where
    res = L.toStrict $ substitute tplt ctx
    ctx          :: T.Text -> T.Text
    ctx "class"  = cls
    ctx "label"  = id
    ctx "number" = T.pack $ show n
    ctx "file"   = T.append prefix  (get "file" kvs)
    ctx str      = get str kvs

get k kvs = fromMaybe (error $ T.unpack $ "Cannot find: " <> k )
            $ lookup k kvs

----------------------------------------------

data Info = Info { chapter :: Int
                 , count   :: Int
                 , label   :: M.Map String Int
                 }
            deriving (Show)

data Ref  = Ref Int Int

instance Show Ref where
  show (Ref i j) = show i ++ "." ++ show j

emptyInfo
  = Info 0 1 M.empty

getCount r id
  = do info <- readIORef r
       let idS = T.unpack id 
       let m  = label info
       let c  = chapter info
       let i  = count info
       let n  = M.findWithDefault i idS m
       let i' = if i == n then i + 1 else i
       let l  = Ref c n
       writeIORef r (info {count = i', label = M.insert idS n m})
       return l -- $ trace ("GETCOUNT: " ++ show l) l

newChapter r
  = do info <- readIORef r
       writeIORef r (info {count = 1, chapter = 1 + chapter info})
