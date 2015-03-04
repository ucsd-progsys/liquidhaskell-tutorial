WEB=web
MAKETEMPLATE=templates/Toc.hs
TOC=src/
METATEMPLATE=templates/pagemeta.template
TEMPLATE=templates/pagegen.template

PANDOCPDF=pandoc \
	--highlight-style=tango \
	--from=markdown+lhs \
	--biblio templates/sw.bib \
	--chapters \
	--latex-engine=pdflatex \
	--template=templates/default.latex \
	--filter templates/Figures.hs \
	--filter templates/inside.hs

PANDOCHTML=pandoc \
     --from=markdown+lhs \
	   --to=html5 \
     -s --mathjax \
	   --standalone \
     --parse-raw \
	   --mathjax \
	   --section-divs \
		 --filter $(WEB)/templates/codeblock.hs \
	   --filter templates/Figures.hs \
	   --filter templates/Links.hs \
	   --filter templates/html.hs \
     --variable=notitle \
     --highlight-style=tango\
     --template=$(TEMPLATE)

PANDOCT=pandoc --from=markdown --to=html --standalone

####################################################################

lhsObjects  := $(wildcard src/*.lhs)
texObjects  := $(patsubst %.lhs,%.tex,$(wildcard src/*.lhs))
htmlObjects := $(patsubst %.lhs,%.html,$(wildcard src/*.lhs))

####################################################################

all: book 

book: $(lhsObjects)
	cat $(lhsObjects) > dist/pbook.lhs
	PANDOC_TARGET=pbook.pdf $(PANDOCPDF) dist/pbook.lhs -o dist/pbook.pdf

web: $(htmlObjects) 
	mv src/*.html $(WEB)/dist/

src/%.html: src/%.lhs template
	PANDOC_TARGET=$@ $(PANDOCHTML) templates/preamble.lhs $? templates/bib.lhs -o $@

site: template
	PANDOC_TARGET=dist.html $(PANDOCHTML) templates/preamble.lhs src/01-intro.lhs templates/bib.lhs -o $(WEB)/dist/foo.html

template:
	$(MAKETEMPLATE) $(METATEMPLATE) $(TOC) $(TEMPLATE)

clean:
	rm -rf dist/* && rm -rf $(WEB)/dist/*.html && rm -rf src/*.tex


###################################################################
####################################################################
####################################################################
####################################################################
####################################################################
####################################################################
####################################################################

LHS2TEX=pandoc \
	--from=markdown+lhs \
	--to=latex \
	--chapters


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


