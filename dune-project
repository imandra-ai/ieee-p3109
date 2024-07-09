(lang dune 3.3)

(name p3109)

(version 0.1)

(authors "Christoph M. Wintersteiger")

(maintainers "Christoph M. Wintersteiger")

(source
 (github imandra-ai/ieee-p3109))

(generate_opam_files true)

(package
 (name imandrax-prelude)
 (synopsis "Imandrax prelude")
 (description "Imandrax prelude")
 (depends containers zarith))

(package
 (name p3109)
 (synopsis "P3109: Arithmetic Formats for Machine Learning")
 (description "P3109: Arithmetic Formats for Machine Learning")
 (depends imandrax-prelude dune ocamlformat))

(dialect
 (name imandrax)
 (implementation
  (extension iml)
  (preprocess
   (run imandrax-cli extract %{input-file} --no-prelude))
  (format
   (system "ocamlformat %{input-file}")))
 (interface
  (extension "imli")))
