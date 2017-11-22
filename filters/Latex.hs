{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-@ LIQUID "--no-termination" @-}

module Main (main) where

import Text.Pandoc.JSON
import Text.Pandoc
import Data.Char (isSpace)
import Data.List
import Data.Monoid (mempty)
import Debug.Trace
import Text.Printf (printf)
import Data.Text (Text, unpack)
import Data.Either (either)

main :: IO ()
main = toJSONFilter readFootnotes

----------------------------------------------------------------------------------
----------------------------------------------------------------------------------
----------------------------------------------------------------------------------

readFootnotes :: Block -> Block
readFootnotes (Div (id, [cls], _) bs)
  | cls `elem` mydivs                 = RawBlock (Format "tex") $ toLaTeX cls id bs
readFootnotes i                       = i

toLaTeX :: String -> String -> [Block] -> String
toLaTeX cls id                        = wrapLatex cls id . either errHandler unpack . runPure . writeLaTeX def . Pandoc mempty
  where errHandler = error $ mconcat ["toLaTex: ", cls, " ", id]

wrapLatex :: String -> String -> String -> String
wrapLatex "footnotetext" _ str        = printf "\\footnotetext{%s}" str
wrapLatex "hwex" name str             = printf "\\begin{hwex}[%s]\n%s\n\\end{hwex}" name str
wrapLatex cls name str                = error $ printf "WrapLatex: %s %s" cls name

mydivs :: [String]
mydivs = ["footnotetext", "hwex"]


-- fnString = "\\footnotetext{"
-- footnoteText :: Inline -> Maybe String
-- footnoteText (RawInline (Format "tex") s) =
--   if fnString `isPrefixOf` s
--     then Just . safeInit . drop (length fnString) $ s -- Remove closing brace
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
expandCodeBlock (CodeBlock a s) = CodeBlock a `fmap` words s

-------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------
data Token = Word String | WhiteSpace String deriving (Eq, Ord, Show)

stringTokens   :: String -> [Token]
stringTokens s = go [] s
  where
    go acc ""  = reverse acc
    go acc s   = let (w, s')   = span (not . isSpace) s
                     (sp, s'') = span (isSpace) s'
                 in
                     go ((WhiteSpace sp) : (Word w) : acc) s''

tokensString :: [Token] -> String
tokensString = concat . map tokenString

tokenString :: Token -> String
tokenString (Word s)       = s
tokenString (WhiteSpace s) = s

