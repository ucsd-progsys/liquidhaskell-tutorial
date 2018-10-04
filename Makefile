RSYNC=$(shell pwd)/sync.sh
remoteuser=rjhala
remotedir=/home/rjhala/public_html/liquid/book
remotehost=goto.ucsd.edu
# TMPDIR=~/tmp/

LIQUIDCLIENT=../liquid-client

METATEMPLATE=templates/pagemeta.template
INDEXTEMPLATE=templates/index.template
PREAMBLE=templates/preamble.lhs
BIB=templates/bib.lhs

# generated
PAGETEMPLATE=dist/page.template
LINKS=dist/links.txt
INDEX=dist/index.lhs

# bin
PANDOC=stack exec -- pandoc
INDEXER=stack exec -- runghc filters/Toc.hs


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
  --filter $(LIQUIDCLIENT)/templates/codeblock.hs \
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
	cp -r $(LIQUIDCLIENT)/fonts _site/
	cp -r $(LIQUIDCLIENT)/css   _site/
	cp -r $(LIQUIDCLIENT)/js    _site/

thing: dist/index.lhs src/00-temp.html
	mv src/00-*.html _site/

indexhtml: $(INDEX)
	$(PANDOC) --from=markdown+lhs --to=html5 --template=$(INDEX) $(PREAMBLE) -o _site/index.html

$(INDEX):
	$(INDEXER) src/ $(METATEMPLATE) $(INDEXTEMPLATE) $(PAGETEMPLATE) $(INDEX) $(LINKS)

src/%.html: src/%.lhs
	PANDOC_TARGET=$@ PANDOC_CODETEMPLATE=$(LIQUIDCLIENT)/templates/code.template $(PANDOCHTML) --template=$(PAGETEMPLATE) $(PREAMBLE) $? templates/bib.lhs -o $@

clean:
	rm -rf dist/* && rm -rf _site/* && rm -rf src/*.tex && rm -rf src/.liquid && rm -rf src/*.html

upload: html
	git checkout master
	make html
	cp -r _site $(TMPDIR)
	git checkout gh-pages
	cp -r $(TMPDIR)/_site/* .
	git commit -a
	git push origin gh-pages
