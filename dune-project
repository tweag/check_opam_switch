(lang dune 3.3)
(name check_opam_switch)
(implicit_transitive_deps false)

(generate_opam_files true)

(package
 (name check_opam_switch)
 (synopsis "A tool to check that the current opam environment is up to date with an opam.export file")
 (description "A tool to check that the current opam environment is up to date with an opam.export file")
 (maintainers "yves.stan.lecornec@tweag.io")
 (depends
  (opam-core (>= 2.0.0))
  (opam-format (>= 2.0.0))
  (minicli (>= 5.0.0))
))

