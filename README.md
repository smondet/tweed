# Tweed: Experiments With Nottui

A few experiments on top of Nottui:

- `open Tweed.Std` for a (hopefully) cleaner API.
- Module `Modal_shortcuts` to make keyboard-based menus mixing Vim-like keys and
  `dmenu`-like search.
- WIP `Debug` and `Errors` widgets.

## Build

```sh
opam_switch_name=tweed51
opam switch create "$opam_switch_name" \
     --formula='"ocaml-base-compiler" {>= "5.1" & < "5.2"}'
opam switch link "$opam_switch_name" .
opam install fmt base nottui ppx_base ppx_deriving ppx_fields_conv ppx_variants_conv ppx_inline_test ppx_expect
```


## Example

Cf. `test/basic.ml`; in a terminal:

```sh
dune exec ./test/basic.exe
```


