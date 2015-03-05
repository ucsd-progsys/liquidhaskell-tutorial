RSYNC=$(shell pwd)/sync.sh
remoteuser=rjhala
remotedir=/home/rjhala/public_html/liquid/book
remotehost=goto.ucsd.edu

LIQUIDCLIENT=../liquid-client
INDEXER=filters/Toc.hs

METATEMPLATE=templates/pagemeta.template
INDEXTEMPLATE=templates/index.TEMPLATE

# generated
PAGETEMPLATE=dist/page.template
LINKS=dist/links.txt
INDEX=dist/index.lhs


##############################################

PANDOCPDF=pandoc \
	--highlight-style=tango \
	--from=markdown+lhs \
	--biblio templates/sw.bib \
	--chapters \
	--latex-engine=pdflatex \
	--template=templates/default.latex \
	--filter filters/Figures.hs \
	--filter filters/Latex.hs

PANDOCHTML=pandoc \
     --from=markdown+lhs \
	 --to=html5 \
     -s --mathjax \
	 --standalone \
     --parse-raw \
	 --mathjax \
	 --section-divs \
	 --filter $(LIQUIDCLIENT)/templates/codeblock.hs \
	 --filter filters/Figures.hs \
	 --filter filters/Html.hs \
	 --variable=notitle \
	 --highlight-style=tango

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

web: indexhtml $(htmlObjects)
	mv src/*.html      _site/
	cp -r img          _site/
	cp -r $(LIQUIDCLIENT)/fonts _site/
	cp -r $(LIQUIDCLIENT)/css   _site/
	cp -r $(LIQUIDCLIENT)/js    _site/

indexhtml: $(INDEX)
	pandoc --from=markdown+lhs --to=html5 --template=$(INDEX) templates/preamble.lhs -o _site/index.html

$(INDEX):
	$(INDEXER) src/ $(METATEMPLATE) $(INDEXTEMPLATE) $(PAGETEMPLATE) $(INDEX) $(LINKS) 

src/%.html: src/%.lhs
	PANDOC_TARGET=$@ PANDOC_CODETEMPLATE=$(LIQUIDCLIENT)/templates/code.template $(PANDOCHTML) --template=$(PAGETEMPLATE) templates/preamble.lhs $? templates/bib.lhs -o $@

clean:
	rm -rf dist/* && rm -rf _site/* && rm -rf src/*.tex && rm -rf src/.liquid && rm -rf src/*.html

rsync:
	$(RSYNC) _site/ $(remoteuser) $(remotehost) $(remotedir)
