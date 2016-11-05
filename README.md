Lyonesse - collection of modules for (Chez) scheme
==================================================

This repository now contains these modules:
- testing
- munsch: a numeric library for R6RS scheme. The name is a contraction of Num-eric Sch-eme,
  with Num reversed, because we schemers like to.
- yasos
- parsing: currently parses XML, it is trivial to add json, cson, and other formats.
- streams: srfi-41 verbatim.
- match: Pattern matching by Dan Friedman, Erik Hilsdale and Kent Dybvig; verbatim.
- record-with-context: add context management to R6 records, access elements with `with-<record>`,
  and update elements (by copy) with `update-<record>`.
- cut: srfi-26 verbatim.

Prerequisites
-------------

A R6RS compatible scheme with a decent set of SRFIs installed; I'm currently using Chez (http://github.com/cisco/chezscheme). Tests also run successfully on Racket. To have them run on Guile, I needed to change '(srfi :48)' into '(ice-9 format)' and it doesn't parse R6RS strings correctly.

Experiments
-----------

- Talking to Gtk3 libraries

Licence
-------
Apache 2.0

