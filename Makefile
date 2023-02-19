#
# Makefile for acmart package
#
# This file is in public domain
#
# $Id: Makefile,v 1.10 2016/04/14 21:55:57 boris Exp $
#

default: tfp.pdf

CC = xelatex # pdflatex
BC = biber # bibtex

.PHONY: clean
clean:
	-rm tfp.aux tfp.log tfp.pdf tfp.bbl

squeaky:
	-rm tfp.aux
	-rm tfp.log
	-rm tfp.pdf
	-rm tfp.bcf
	-rm tfp.blg
	-rm tfp.toc
	-rm tfp.bbl
	-rm tfp.idx
	-rm tfp.out
	-rm tfp.run.xml


supersqueaky:
	- biber --cache
	- echo "clear biber's cache"

tfp.pdf: tfp.tex tfp.bib
	$(CC) --shell-escape tfp.tex
	$(BC) --validate_datamodel --isbn-normalise --debug --trace tfp
	$(CC) --shell-escape tfp.tex
	$(CC) --shell-escape tfp.tex


grammar: tfp.tex
	detex tfp.tex | tee >(style -L en -n -r 9) >(diction -L en -s) >/dev/null | less
