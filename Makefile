PANDOC=pandoc --highlight-style=tango --from=markdown+lhs --biblio templates/sw.bib --chapters --latex-engine=pdflatex --template=templates/default.latex --filter templates/inside.hs

LHS2TEX=pandoc \
	--from=markdown+lhs \
	--to=latex \
	--chapters

## CITATION HACKERY
# pandoc --from=markdown+lhs --chapters --latex-engine=pdflatex --template=templates/default.latex --filter templates/inside.hs --metadata bibliography=sw.bib dist/pbook.lhs -o dist/pbook.pdf


lhsObjects  := $(wildcard src/*.lhs)
texObjects  := $(patsubst %.lhs,%.tex,$(wildcard src/*.lhs))

all: pbook

pbook: $(lhsObjects)
	cat $(lhsObjects) > dist/pbook.lhs
	$(PANDOC) dist/pbook.lhs -o dist/pbook.pdf

json: $(lhsObjects)
	cat $(lhsObjects) > dist/pbook.lhs
	$(PANDOC) dist/pbook.lhs -t json


book: templates/book.tex $(texObjects) 
	cp templates/book.tex dist/
	mv src/*.tex dist/
	cd dist/ && pdflatex book.tex && cd ..

src/%.tex: src/%.lhs
	-$(LHS2TEX) $? -o $@ 

clean:
	rm -rf dist/* && rm -rf src/*.tex


