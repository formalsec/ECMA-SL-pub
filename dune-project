(lang dune 3.2)

(using menhir 2.1)

(using dune_site 0.1)

(using mdx 0.2)

(name ecma-sl)

(generate_opam_files true)

(source
 (github formalsec/ECMA-SL))

(authors "José Fragoso Santos")

(maintainers "José Fragoso Santos")

(license "GPL-3.0-or-later")

(documentation https://github.com/formalsec.github.io/ECMA-SL)

(package
 (name ecma-sl)
 (synopsis "ECMA-SL")
 (description
  "A platform for specifying and running the ECMAScript standard.")
 (depends
  (ocaml
   (>= "5.0.0"))
  (odoc :with-doc)
  dune
  dune-site
  bos
  eio
  eio_main
  cmdliner
  curses
  cohttp
  cohttp-lwt-unix
  lwt
  menhir
  menhirLib
  tls-lwt
  yojson
  conf-npm
  (smtml
   (= "0.7.0"))
  ppx_import
  ppx_deriving
  (mdx :with-test)
  (ppx_inline_test :with-test)
  (prelude
   (>= "0.5"))
  (rusage :with-test)
  (bisect_ppx
   (and
    (>= "2.5.0")
    :with-test
    :with-dev-setup)))
 (tags
  (ECMAScript "Specification Language" "Reference Interpreters" "OCaml"))
 (sites
  (share interpreters)
  (share stdlib)))
