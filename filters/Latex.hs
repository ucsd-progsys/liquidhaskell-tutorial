{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-@ LIQUID "--no-termination" @-}

module Latex (main) where

import Text.Pandoc.JSON
import Text.Pandoc
import Data.Char (isSpace)
import Data.List
import Data.Monoid (mempty)
import Debug.Trace
import Text.Printf (printf)
import Data.Text (Text, unpack, pack)
import qualified Data.Text as T
import Data.Either (either)

main :: IO ()
main = toJSONFilter readFootnotes

----------------------------------------------------------------------------------
----------------------------------------------------------------------------------
----------------------------------------------------------------------------------

readFootnotes :: Block -> Block
readFootnotes (Div (id, [cls], _) bs)
  | cls `elem` mydivs                 = RawBlock (Format "tex") (toLaTeX cls id bs)
readFootnotes i                       = i

toLaTeX :: Text -> Text -> [Block] -> Text
toLaTeX cls id                        = wrapLatex cls id . either errHandler (\x -> x) . runPure . writeLaTeX def . Pandoc mempty
  where errHandler = error $ unpack $ mconcat ["toLaTex: ", cls, " ", id]

wrapLatex :: Text -> Text -> Text -> Text 
wrapLatex "footnotetext" _ str        = T.pack $ printf "\\footnotetext{%s}" (T.unpack str)
wrapLatex "hwex" name str             = T.pack $ printf "\\begin{hwex}[%s]\n%s\n\\end{hwex}" (T.unpack name) (T.unpack str)
wrapLatex cls name str                = error $ printf "WrapLatex: %s %s" cls name

mydivs :: [Text]
mydivs = ["footnotetext", "hwex"]

-- fnText = "\\footnotetext{"
-- footnoteText :: Inline -> Maybe Text
-- footnoteText (RawInline (Format "tex") s) =
--   if fnText `isPrefixOf` s
--     then Just . safeInit . drop (length fnText) $ s -- Remove closing brace
--     else Nothing
--
-- footnoteText x = Nothing




-----------------------------------------------------------------------------------------

safeInit [] = []
safeInit xs = init xs

-----------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------


-- main = toJSONFilter txBlock

-- bb = CodeBlock ("",["sourceCode","literate","haskell"],[]) "ranjit :: Int\nranjit = 12 + flibbertypopp \n\nflibbertypopp :: Int\nflibbertypopp = 42"

txBlock :: Maybe Format -> Block -> [Block]
txBlock _ cb@(CodeBlock _ _) = expandCodeBlock cb
txBlock _ b                  = [b]

expandCodeBlock :: Block -> [Block]
expandCodeBlock (CodeBlock a s) = CodeBlock a `fmap` tWords s
  where 
    tWords = fmap pack . words . unpack
-------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------
data Token = Word Text | WhiteSpace Text deriving (Eq, Ord, Show)

stringTokens   :: Text -> [Token]
stringTokens s = go [] s
  where
    go acc ""  = reverse acc
    go acc s   = let (w, s')   = T.span (not . isSpace) s
                     (sp, s'') = T.span (isSpace) s'
                 in
                     go ((WhiteSpace sp) : (Word w) : acc) s''

tokensText :: [Token] -> Text
tokensText = mconcat . map tokenText

tokenText :: Token -> Text
tokenText (Word s)       = s
tokenText (WhiteSpace s) = s

