WEB=web

PANDOCPDF=pandoc \
	--highlight-style=tango \
	--from=markdown+lhs \
	--biblio templates/sw.bib \
	--chapters \
	--latex-engine=pdflatex \
	--template=templates/default.latex \
	--filter templates/inside.hs


PANDOCHTML=pandoc \
     --from=markdown+lhs \
	   --to=html5 \
	   --standalone \
	   --mathjax \
	   --section-divs \
		 --filter $(WEB)/templates/codeblock.hs \
     --highlight-style=tango\
     --template=$(WEB)/templates/page.template

####################################################################


LHS2TEX=pandoc \
	--from=markdown+lhs \
	--to=latex \
	--chapters

lhsObjects  := $(wildcard src/*.lhs)
texObjects  := $(patsubst %.lhs,%.tex,$(wildcard src/*.lhs))

all: book

book: $(lhsObjects)
	cat $(lhsObjects) > dist/pbook.lhs
	$(PANDOCPDF) dist/pbook.lhs -o dist/pbook.pdf

site: 
	$(PANDOCHTML) src/01-intro.lhs -o web/dist/foo.html






clean:
	rm -rf dist/* && rm -rf src/*.tex



## CITATION HACKERY
# pandoc --from=markdown+lhs --chapters --latex-engine=pdflatex --template=templates/default.latex --filter templates/inside.hs --metadata bibliography=sw.bib dist/pbook.lhs -o dist/pbook.pdf

###### json: $(lhsObjects)
###### 	cat $(lhsObjects) > dist/pbook.lhs
###### 	$(PANDOC) dist/pbook.lhs -t json
###### 
###### 
###### book: templates/book.tex $(texObjects) 
###### 	cp templates/book.tex dist/
###### 	mv src/*.tex dist/
###### 	cd dist/ && pdflatex book.tex && cd ..
###### 
###### src/%.tex: src/%.lhs
###### 	-$(LHS2TEX) $? -o $@ 


