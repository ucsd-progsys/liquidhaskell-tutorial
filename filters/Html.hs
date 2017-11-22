{-@ LIQUID "--no-termination" @-}

module Main (main) where

import Data.Maybe
import Text.Pandoc.JSON
-- import Text.Pandoc
-- import Data.Char (isSpace)
import Data.List
-- import Data.Monoid (mempty)
import Debug.Trace
-- import Text.Printf (printf)
-- import Text.Pandoc.Walk (walk)

main :: IO ()
main = toJSONFilter tx

tx :: Block -> Block
tx (Para is)
  = Para $ concatMap txInline is
tx (Plain is)
  = Plain $ concatMap txInline is
tx (Div (id, [cls], kvs) bs)
  = toHTML cls id kvs bs
tx z
  = z -- trace ("BLOCK:" ++ show z) z

txInline (RawInline (Format "tex") z)
  | isNoindent z
  = []

  | isHint z
  = [Strong [Str "Hint: "]]

  | isNewthought z
  = [LineBreak, Strong [Str z']]
  where z' = stripLatexCmd z

-- txInline (RawInline (Format "tex") z)
--   = error $ "BAD LATEX: " ++ show z

txInline i
  = [i']
  where i' = {- trace ("INLINE:" ++ show i) -} i

toHTML "hwex" id kvs (b : bs)
  = Div (id, ["hwex"], kvs) bs'
    where
      bs' = (addHdr hdr b) : bs ++ [Plain [LineBreak, LineBreak]]
      hdr = Strong [Str $ "Exercise: (" ++ id ++ "): "]

toHTML cls id kvs bs
  = Div (id, [cls], kvs)
    $ trace ("TODO toHTML: " ++ show (id, cls))
    $ bs

addHdr i (Plain is) = Plain (LineBreak : i : is)
addHdr i (Para is)  = Para  (LineBreak : i : is)

stripLatexCmd :: String -> String
stripLatexCmd = snipMaybe . dropWhile (/= '{')

snipMaybe z = fromMaybe z $ snip z

dropBrace ('{':s) = dropLast s
dropBrace s       = s

dropLast  = reverse . dropFirst . reverse
dropFirst = tail

isNoindent   = isPrefixOf "\\noindent"
isNewthought = isPrefixOf "\\newthought"
isHint       = isPrefixOf "\\hint"

txVerb s
  | isVerb c0 cn = error "asd" -- CodeBlock ... (unlines $ fromMaybe [] $ snip ls)
  | otherwise    = s
  where
    ls           = lines s
    l0           = head ls
    ln           = last ls
    c0           = stripLatexCmd l0
    cn           = stripLatexCmd ln

isVerb c0 cn = c0 == cn && c0 `elem` ["ghci", "verbatim", "shell"]

snip xs@(_:_:_) = Just $ dropLast $ dropFirst xs
snip _          = Nothing

-- BLOCK:CodeBlock ("",["shell"],[]) "$ ls -al"

convertFile inF outF = writeFile outF . convertStr =<< readFile inF

convertStr  = unlines . map convertLine . lines

convertLine = go conversions
  where
    go ((x, y):rs) s
      | isPrefixOf x s = y
      | otherwise      = go rs s
    go []          s   = s

conversions
  = [ ("\\begin{shell}"       , "~~~~~{.sh}")
    , ("\\end{shell}"         , "~~~~~")
    , ("\\begin{ghci}"        , "~~~~~{.ghci}")
    , ("\\end{ghci}"          , "~~~~~")
    , ("\\begin{spec}"        , "~~~~~{.spec}")
    , ("\\end{spec}"          , "~~~~~")
    , ("\\begin{liquiderror}" , "~~~~~{.liquiderror}")
    , ("\\end{liquiderror}"   , "~~~~~")
    ]
