#!/bin/bash

mkHTML() {
   srcFile=$1
  
   echo "HTML for $srcFile"

   PANDOC_TARGET=src/$srcFile.html PANDOC_CODETEMPLATE=../liquid-client/templates/code.template stack exec -- pandoc --from=markdown+lhs+raw_html --to=html5 -s --mathjax --standalone --mathjax --toc --section-divs --filter filters/Codeblock.hs --filter filters/Figures.hs --filter filters/Html.hs --variable=notitle --highlight-style=tango --template=dist/page.template templates/preamble.lhs src/$srcFile.lhs templates/bib.lhs -o src/$srcFile.html
}


mkLHS() { 
   echo "Build single LHS file"
   cat src/Tutorial_01_Introduction.lhs src/Tutorial_02_Logic.lhs src/Tutorial_03_Basic.lhs src/Tutorial_04_Polymorphism.lhs src/Tutorial_05_Datatypes.lhs src/Tutorial_06_Measure_Bool.lhs src/Tutorial_07_Measure_Int.lhs src/Tutorial_08_Measure_Set.lhs src/Tutorial_09_Case_Study_Lazy_Queues.lhs src/Tutorial_10_Case_Study_Associative_Maps.lhs src/Tutorial_11_Case_Study_Pointers.lhs src/Tutorial_12_Case_Study_AVL.lhs > dist/pbook.lhs
}

mkPDF () {
  echo "Build PDF"
  PANDOC_TARGET=pbook.pdf stack exec -- pandoc --highlight-style=tango --from=markdown+lhs --bibliography=templates/sw.bib --biblatex --top-level-division=chapter --template=templates/default.latex --filter filters/Figures.hs --filter filters/Latex.hs templates/preamble.lhs templates/bib.lhs dist/pbook.lhs -o dist/pbook.pdf
}

mkWeb() {
  mkHTML Tutorial_01_Introduction
  mkHTML Tutorial_02_Logic
  mkHTML Tutorial_03_Basic
  mkHTML Tutorial_04_Polymorphism
  mkHTML Tutorial_05_Datatypes
  mkHTML Tutorial_06_Measure_Bool
  mkHTML Tutorial_07_Measure_Int
  mkHTML Tutorial_08_Measure_Set
  mkHTML Tutorial_09_Case_Study_Lazy_Queues
  mkHTML Tutorial_10_Case_Study_Associative_Maps
  mkHTML Tutorial_11_Case_Study_Pointers
  mkHTML Tutorial_12_Case_Study_AVL
}

mkGITHUB() {
  cp dist/pbook.pdf _site/book.pdf
  cp -r _site ~/tmp/
  git checkout gh-pages
  cp -r ~/tmp/_site/* .
  git commit -a -m "updating GH-PAGES"
  git push origin gh-pages
  git checkout main
}

mkdir -p dist
mkLHS
mkPDF
mkWEB
mkGITHUB
