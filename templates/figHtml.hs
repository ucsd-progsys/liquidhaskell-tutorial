{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-@ LIQUID "--no-termination" @-}

module FigHtml where

import Data.IORef
import Text.Pandoc.JSON
import Text.Pandoc
import Text.Pandoc.Walk (walkM)
-- import Data.Char (isSpace)
import Data.List (isSuffixOf, isPrefixOf)
-- import Data.Monoid (mempty)
import Debug.Trace
import Text.Printf (printf)
import System.Directory
import System.IO
import Control.Applicative ((<$>))

import qualified Data.ByteString.Lazy as S
import qualified Data.Text as T
import qualified Data.Text.Lazy as L
import qualified Data.Text.IO as TIO
import qualified Data.Map as M
import Data.Maybe (fromMaybe)

import Data.Text.Template


prefix = "../../"

main :: IO ()
main = do r    <- newIORef M.empty 
          tplt <- TIO.readFile "templates/figHtml.template"
          toJSONFilter (tx tplt r)

tx t r b0
  = do b1 <- txBlock t r b0
       b2 <- txLink    r b1
       return b2
              
txLink r = walkM (reLink r)

reLink   :: IORef (M.Map String Int) -> Inline -> IO Inline
reLink r (Link [Str "auto"] tgt@('#':id,_))
  = do n <- getCount r id
       return $ Link [Str (show n)] tgt

reLink _ i
  = return i
  
txBlock :: T.Text -> IORef (M.Map String Int) -> Block -> IO Block

txBlock t r (Div (id, [cls], kvs) _)
  | isFigure cls
  = makeFigure t r id cls kvs 
      
txBlock _ _ z
  = return z -- $ trace ("IAMTHIS:" ++ show z) z

isFigure s    = s `elem` ["figure", "marginfigure"]

makeFigure t r id cls kvs
  = RawBlock (Format "html") . pad t id cls kvs <$> getCount r id
    
getCount r id
  = do m    <- readIORef r
       let n = M.findWithDefault (1 + M.size m) id m
       writeIORef r (M.insert id n m)
       return n

pad tplt id cls kvs n
  = L.unpack $ substitute tplt ctx 
  where
    ctx          :: T.Text -> T.Text 
    ctx "label"  = T.pack id 
    ctx "number" = T.pack $ show n
    ctx "file"   = T.append prefix  (get "file" kvs) 
    ctx str      = get (T.unpack str) kvs

get k kvs = T.pack
            $ fromMaybe (error $ "Cannot find: " ++ k )
            $ lookup k kvs

