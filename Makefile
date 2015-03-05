RSYNC=$(shell pwd)/sync.sh
remoteuser=rjhala
remotedir=/home/rjhala/public_html/liquid/book
remotehost=goto.ucsd.edu

WEB=web
INDEXER=templates/Toc.hs
TOC=src/

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
	   --filter templates/html.hs \
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
	mv src/*.html $(WEB)/dist/

site:
	PANDOC_TARGET=dist.html $(PANDOCHTML) --template=$(PAGETEMPLATE) templates/preamble.lhs src/01-intro.lhs templates/bib.lhs -o $(WEB)/dist/foo.html

indexhtml: $(INDEX)
	pandoc --from=markdown+lhs --to=html5 --template=$(INDEX) templates/preamble.lhs -o dist/index.html
	mv dist/index.html $(WEB)/dist/

$(INDEX):
	$(INDEXER) $(TOC) $(METATEMPLATE) $(INDEXTEMPLATE) $(PAGETEMPLATE) $(INDEX) $(LINKS) 


src/%.html: src/%.lhs
	PANDOC_TARGET=$@ $(PANDOCHTML) --template=$(PAGETEMPLATE) templates/preamble.lhs $? templates/bib.lhs -o $@

clean:
	rm -rf dist/* && rm -rf $(WEB)/dist/*.html && rm -rf src/*.tex

rsync:
	$(RSYNC) $(WEB) $(remoteuser) $(remotehost) $(remotedir)
