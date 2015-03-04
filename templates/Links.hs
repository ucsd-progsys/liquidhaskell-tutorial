{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-@ LIQUID "--no-termination" @-}

module Links where

-- import Data.Char (isSpace)
-- import Data.Monoid (mempty)
-- import System.Environment (getEnv)
 
import Text.Pandoc.JSON
import Debug.Trace

-- import Data.IORef
-- import Text.Pandoc
import Text.Pandoc.Walk (walk)
-- import Data.List (isSuffixOf, isPrefixOf)
-- import Text.Printf (printf)
--  
-- 
-- import System.Directory
-- import System.IO
-- import Control.Applicative ((<$>))
-- 
-- import qualified Data.ByteString.Lazy as S
-- import qualified Data.Text as T
-- import qualified Data.Text.Lazy as L
-- import qualified Data.Text.IO as TIO
-- import qualified Data.Map as M
-- import Data.Maybe (fromMaybe)
-- import System.Environment (getEnv)
-- import Data.Text.Template

main :: IO ()
main = toJSONFilter tx

tx :: Block -> Block
tx b@(Header n a xs)  = trace ("HEADER: " ++ show (n, a, xs)) $ b
tx b                  = walk showLink b

showLink :: Inline -> Inline
showLink i@(Link x y) = trace ("LINK: " ++ show (x, y)) i
showLink i            = i 
                       
