(lang dune 3.6)

(name icpc_domestic2021_c)

(generate_opam_files true)

(source
 (github username/reponame))

(authors "Author Name")

(maintainers "Maintainer Name")

(license LICENSE)

(documentation https://url/to/documentation)

(using menhir 2.1)

(package
 (name icpc_domestic2021_c)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends ocaml dune menhir ocamlgraph)
 (tags
  (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
