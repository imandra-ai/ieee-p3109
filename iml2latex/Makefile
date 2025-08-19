FUNS=$(shell grep "let aug_" ../specification/specification.iml | grep -v is_within | cut -d " " -f 4)
TEXS=$(patsubst %,%.tex,$(FUNS))

all: spec.pdf

spec.pdf: node_modules spec.tex $(TEXS)
	pdflatex spec

spec.tex:
	:

%.tex: ../specification/specification.iml node_modules *.ts *.js
	npx tsx iml2latex.ts ../specification/specification.iml $(patsubst %.tex,%,$@) > $@
	sed -i -e :a -e '/^\n*$$/{$$d;N;};/\n$$/ba' $@

node_modules:
	npm install