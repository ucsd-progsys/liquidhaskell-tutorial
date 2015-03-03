{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-@ LIQUID "--no-termination" @-}

module Figures where

import Data.IORef
import Text.Pandoc.JSON
import Text.Pandoc
import Text.Pandoc.Walk (walkM)
import Data.List (isSuffixOf, isPrefixOf)
import Debug.Trace
import Text.Printf (printf)
 
-- import Data.Char (isSpace)
-- import Data.Monoid (mempty)
-- import System.Environment (getEnv)
 
import System.Directory
import System.IO
import Control.Applicative ((<$>))

import qualified Data.ByteString.Lazy as S
import qualified Data.Text as T
import qualified Data.Text.Lazy as L
import qualified Data.Text.IO as TIO
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import System.Environment (getEnv)

import Data.Text.Template

main :: IO ()
main = do tgt <- output <$> getEnv "PANDOC_TARGET"
          -- trace ("Pandoc OUTPUT: " ++ show tgt)
          txFig tgt

data Output = HTML | LATEX deriving (Eq)

instance Show Output where
  show HTML  = "html"
  show LATEX = "latex" 

output "html"  = HTML
output "latex" = LATEX
output s       = error $ "Figures : unknown target: " ++ s 

txFig HTML  = txFigures HTML  "../../" "templates/figHtml.template"
txFig LATEX = txFigures LATEX ""       "templates/figLatex.template"

txFigures :: Output -> FilePath -> FilePath -> IO () 
txFigures tgt prefix templateF 
  = do r    <- newIORef M.empty 
       tplt <- TIO.readFile templateF 
       toJSONFilter (tx tgt (T.pack prefix) tplt r)


tx tgt prefix t r b0
  = do b1 <- txBlock tgt prefix t r b0
       b2 <- txLink               r b1
       return b2

              
txLink r = walkM (reLink r)

reLink   :: IORef (M.Map String Int) -> Inline -> IO Inline
reLink r (Link [Str "auto"] tgt@('#':id,_))
  = do n <- getCount r id
       return $ Link [Str (show n)] tgt

reLink _ i
  = return i


txBlock tgt prefix t r (Div (id, [cls], kvs) _)
  | isFigure cls
  = makeFigure tgt prefix t r id cls kvs 
      
txBlock _ _ _ _ z
  = return z -- $ trace ("IAMTHIS:" ++ show z) z

isFigure s    = s `elem` ["figure", "marginfigure"]

makeFigure tgt prefix t r id cls kvs
  = RawBlock (Format $ show tgt) . pad prefix t id cls kvs <$> getCount r id
     
getCount r id
  = do m    <- readIORef r
       let n = M.findWithDefault (1 + M.size m) id m
       writeIORef r (M.insert id n m)
       return n

pad prefix tplt id cls kvs n
  = trace ("PAD" ++ show res) $ res 
  where
    res = L.unpack $ substitute tplt ctx 
    ctx          :: T.Text -> T.Text 
    ctx "class"  = T.pack cls 
    ctx "label"  = T.pack id 
    ctx "number" = T.pack $ show n
    ctx "file"   = T.append prefix  (get "file" kvs) 
    ctx str      = get (T.unpack str) kvs

get k kvs = T.pack
            $ fromMaybe (error $ "Cannot find: " ++ k )
            $ lookup k kvs

