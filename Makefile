src = bdao.rkt
lib = utils.rkt

all: html

.PHONY: all slides html wc mrproper clean

html slides bdao: index.html
index.html: bdao.rkt reveal.rkt
	racket $< > $@.tmp && mv $@.tmp $@ || rm $@.tmp

wc: bdao.wc

%.W: %.html
	w3m -T text/html $<

%.wc: %.html
	perl $$(which donuts.pl) unhtml < $< | wc

clean:
	rm -f *.pdf *.html *.tex *.css *.js
	rm -rf tmp

mrproper:
	git clean -xfd
