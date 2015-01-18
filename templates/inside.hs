{-# LANGUAGE ViewPatterns #-}
{-@ LIQUID "--no-termination" @-}

module InTex where

import Text.Pandoc.JSON
import Text.Pandoc
import Data.Char (isSpace)
import Data.List
import Data.Monoid (mempty)
import Debug.Trace

main :: IO ()
main = toJSONFilter readFootnotes
-- main = toJSONFilter txBlock 

-- bb = CodeBlock ("",["sourceCode","literate","haskell"],[]) "ranjit :: Int\nranjit = 12 + flibbertypopp \n\nflibbertypopp :: Int\nflibbertypopp = 42"


txBlock :: Maybe Format -> Block -> [Block]
txBlock _ cb@(CodeBlock _ _) = expandCodeBlock cb 
txBlock _ b                  = [b]

expandCodeBlock :: Block -> [Block]
expandCodeBlock (CodeBlock a s) = CodeBlock a `fmap` words s

-----------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------
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

-----------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------
readFootnotes :: Inline -> Inline
readFootnotes (footnoteText -> Just args) = RawInline (Format "tex") res
  where
    parsed   = writeLaTeX def . readMarkdown def
    res     = fnString ++ parsed args ++ "}"

readFootnotes (Span (_,["footnotetext"],_) is) = RawInline (Format "tex") tex 
  where
    tex   = fnString ++ writeLaTeX def para ++ "}" 
    para  = Pandoc mempty [Para is]
    
readFootnotes i = i -- trace ("YIKES:" ++ show i)  i  
                  
fnString = "\\footnotetext{"

footnoteText :: Inline -> Maybe String
footnoteText (RawInline (Format "tex") s) =
  if fnString `isPrefixOf` s
    then Just . safeInit . drop (length fnString) $ s -- Remove closing brace
    else Nothing

footnoteText x = Nothing


-----------------------------------------------------------------------------------------

safeInit [] = []
safeInit xs = init xs


              
