NOEXTRACT=width wBlockDecode wBlockProject wDecode wRoundToPrecision wSaturate wBlockProject_map_cz
FUNS=$(shell grep "let w" ../specification/specification.iml | cut -d " " -f 4) internal_wSaturate
TEXS=$(patsubst %,%.tex,$(filter-out $(NOEXTRACT),$(FUNS)))

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