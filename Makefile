RSYNC=$(shell pwd)/sync.sh

METATEMPLATE=templates/pagemeta.template
INDEXTEMPLATE=templates/index.template
PREAMBLE=templates/preamble.lhs
BIB=templates/bib.lhs

# generated
PAGETEMPLATE=dist/page.template
LINKS=dist/links.txt
INDEX=dist/index.lhs

# bin
## INDEXER=stack exec -- runghc filters/Toc.hs
## PANDOC=pandoc
INDEXER=stack $(STACK_FLAGS) run filters-toc
PANDOC=stack $(STACK_FLAGS) exec -- pandoc

##############################################

PANDOCPDF=$(PANDOC) \
	--highlight-style=tango \
	--from=markdown+lhs \
	--bibliography=templates/sw.bib \
	--biblatex \
	--top-level-division=chapter \
	--template=templates/default.latex \
	--filter filters/Figures.hs \
	--filter filters/Latex.hs

PANDOCHTML=$(PANDOC) \
  --from=markdown+lhs+raw_html \
  --to=html5 \
  -s --mathjax \
  --standalone \
  --mathjax \
  --toc \
  --section-divs \
  --filter filters/Codeblock.hs \
  --filter filters/Figures.hs \
  --filter filters/Html.hs \
  --variable=notitle \
  --highlight-style=tango


####################################################################

lhsObjects  := $(sort $(wildcard src/*.lhs))
texObjects  := $(patsubst %.lhs,%.tex,$(wildcard src/*.lhs))
htmlObjects := $(patsubst %.lhs,%.html,$(wildcard src/*.lhs))

####################################################################

all: pdf

pdf: dist/pbook.lhs 
	PANDOC_TARGET=pbook.pdf $(PANDOCPDF) $(PREAMBLE) $(BIB) dist/pbook.lhs -o dist/pbook.pdf

dist/pbook.lhs: $(lhsObjects)
	mkdir -p dist
	cat $(lhsObjects) > dist/pbook.lhs

html: indexhtml $(htmlObjects)
	mv src/*.html      _site/
	cp -r img          _site/
	cp -r fonts _site/
	cp -r css   _site/
	cp -r js    _site/

thing: dist/index.lhs src/00-temp.html
	mv src/00-*.html _site/

indexhtml: $(INDEX)
	$(PANDOC) --from=markdown+lhs --to=html5 --template=$(INDEX) $(PREAMBLE) -o _site/index.html

$(INDEX):
	$(INDEXER) src/ $(METATEMPLATE) $(INDEXTEMPLATE) $(PAGETEMPLATE) $(INDEX) $(LINKS)

src/%.html: src/%.lhs
	PANDOC_TARGET=$@ PANDOC_CODETEMPLATE=templates/code.template $(PANDOCHTML) --template=$(PAGETEMPLATE) $(PREAMBLE) $? templates/bib.lhs -o $@

check:
	stack build --fast --flag liquidhaskell-tutorial:build

check-cabal:
	cabal v2-build

clean:
	rm -rf dist/* && rm -rf _site/* && rm -rf src/*.tex && rm -rf src/.liquid && rm -rf src/*.html

upload: pdf html
	cp dist/pbook.pdf _site/book.pdf
	cp -r _site $(TMPDIR)
	git checkout gh-pages
	cp -r $(TMPDIR)/_site/* .
	git commit -a -m "updating GH-PAGES"
	git push origin gh-pages
	git checkout main
