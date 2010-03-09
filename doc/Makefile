.PHONY: clean html pdf

clean:
	rm -rf include *.aux *.cp *.fn *.fns *.ky *.log *.pg *.toc *.tp *.tps *.vr *.pdf *.html

# Hook into the super sekrit texinfo generator in the SBCL tree -- this is just a quick way
# to bootrap documentation for now.
include:
	sbcl --eval '(progn (require :asdf) (require :alexandria))' \
	--load docstrings.lisp \
	--eval '(sb-texinfo:generate-includes "include/" (list :alexandria) :base-package :alexandria)' \
	--eval '(quit)'

pdf: include
	texi2pdf alexandria.texinfo

html: include
	texi2html alexandria.texinfo
