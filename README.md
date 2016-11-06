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

Munsch - Numeric Scheme
=======================

Munsch is designed to do fast numerical array routines in a flexible way and
semantic way. I'm currently developing it to interact with OpenGL functions.

Goals:
- Complete R6RS compliance, based on bytearrays
- Support for extensive slicing and dicing of numeric arrays
- Support for linear algebra
- Support for geometry

The basis for Munsch is `bytearrays`, on top of which we have a `record-type`s
called `f32vector` and `f32array` (only single precesion for the moment). The
vector type contains a contiguous block of memory, being a simple wrapper
around `bytevector`.  The `f32array` type contains slicing information in
addition to a reference to the raw data. A `slice` contains information on
offset, stride, shape and total size of the array. To perform slicing
operations on an array I have defined the `f32array-cut` syntax.

To perform linear algebra, I defined matrix and vector types for which some of
the well known operations on matrices are defined.  These use `f32vector` to
store their memory, and are always contiguous. This is so that we can pass them
to OpenGL routines, but also applications using FFI to LAPACK or GSL come to
mind.

Next, on top of the linear algebra we can define a vector type that specialises
in geometry operations. This is to provide semantics for vertices, vectors,
polygons etc.

Licence
-------
Apache 2.0

