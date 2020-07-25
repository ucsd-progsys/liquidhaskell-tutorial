#!/bin/bash

mkHTML() {
   srcFile=$1
  
   echo "HTML for $srcFile"

   PANDOC_TARGET=src/$srcFile.html PANDOC_CODETEMPLATE=../liquid-client/templates/code.template stack exec -- pandoc --from=markdown+lhs+raw_html --to=html5 -s --mathjax --standalone --mathjax --toc --section-divs --filter filters/Codeblock.hs --filter filters/Figures.hs --filter filters/Html.hs --variable=notitle --highlight-style=tango --template=dist/page.template templates/preamble.lhs src/$srcFile.lhs templates/bib.lhs -o src/$srcFile.html
}

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
