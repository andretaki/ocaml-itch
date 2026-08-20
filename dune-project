(lang dune 3.16)
(name itch)

(generate_opam_files true)

(source (github andretaki/ocaml-itch))
(license MIT)
(authors "Andre Taki")
(maintainers "Andre Taki")

(package
 (name itch)
 (synopsis "Nasdaq TotalView-ITCH 5.0 parser and order book reconstructor")
 (description
  "A zero-allocation parser for the Nasdaq TotalView-ITCH 5.0 binary market data\nprotocol, plus a limit order book reconstructor built on top of it.")
 (depends
  (ocaml (>= 5.2))
  core
  base_bigstring
  (ppx_jane (>= v0.17))))
