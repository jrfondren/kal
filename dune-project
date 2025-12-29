(lang dune 3.20)
(name kal)
(generate_opam_files true)

(source
 (github jrfondren/kal))

(authors "Julian Fondren <julian.fondren@gmail>")
(maintainers "Julian Fondren <julian.fondren@gmail>")
(license MIT)
(documentation https://github.com/jrfondren/kal)

(package
 (name kal)
 (synopsis "A calorie-counting unix filter")
 (description "A unix filter provides a kind of Emacs mode for diet tracking, with any editor.")
 (depends ocaml unix))
