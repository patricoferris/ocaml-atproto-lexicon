(lang dune 3.17)

(name atproto-lexicon)

(generate_opam_files true)

(source
 (github patricoferris/ocaml-atproto-lexicon))

(authors "Patrick Ferris <patrick@sirref.org>")

(maintainers "Patrick Ferris <patrick@sirref.org>")

(license MIT)

(package
 (name atproto-lexicon)
 (synopsis "AT Protocol Lexicon Toolkit")
 (description "Tools for working with the AT Protocol Lexicon")
 (depends
   ocaml
   ppxlib
   jsont)
 (tags
  ("AT Protocol")))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
