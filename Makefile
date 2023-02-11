#
# Makefile for acmart package
#
# This file is in public domain
#
# $Id: Makefile,v 1.10 2016/04/14 21:55:57 boris Exp $
#

default: mkw.pdf

CC = xelatex # pdflatex
BC = biber # bibtex

.PHONY: clean
clean:
	-rm mkw.aux mkw.log mkw.pdf mkw.bbl

squeaky:
	-rm mkw.aux
	-rm mkw.log
	-rm mkw.pdf
	-rm mkw.bcf
	-rm mkw.blg
	-rm mkw.toc
	-rm mkw.bbl
	-rm mkw.idx
	-rm mkw.out
	-rm mkw.run.xml


supersqueaky:
	- biber --cache
	- echo "clear biber's cache"

mkw.pdf: mkw.tex mkw.bib
	$(CC) --shell-escape mkw.tex
	$(BC) --validate_datamodel --isbn-normalise --debug --trace mkw
	$(CC) --shell-escape mkw.tex
	$(CC) --shell-escape mkw.tex


grammar: mkw.tex
	detex mkw.tex | tee >(style -L en -n -r 9) >(diction -L en -s) >/dev/null | less
