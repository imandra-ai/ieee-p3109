all: report.txt

spec-formats: # ../../specification/specification.ml ../../specification/test/table.ml
	dune exec -- ../../specification/test/table.exe formats-csv spec-formats

env:
	python -m venv env

report.txt: spec-formats
	. env/bin/activate
	python compare.py > $@