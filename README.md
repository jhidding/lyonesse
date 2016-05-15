Lyonesse - collection of modules for (Chez) scheme
==================================================

This repository now contains these modules:
- testing
- oop
- yasos

Prerequisites
-------------

A R6RS compatible scheme with a decent set of SRFIs installed; I'm currently using Chez (http://github.com/cisco/chezscheme). Tests also run successfully on Racket. To have them run on Guile, I needed to change '(srfi :48)' into '(ice-9 format)' and it doesn't parse R6RS strings correctly.

Experiments
-----------

- Talking to Gtk3 libraries

