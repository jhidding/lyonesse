#| SRFI-1 contains many functions that are also in RnRS. We need a
 | port with only the useful extras.
 |#
(library (lyonesse strings)
  (export string-join)
  (import (rnrs base (6))
          (rnrs lists (6))
          (lyonesse functional))

  (define (string-join sep lst)
    (if (null? lst)
      ""
      (fold-left ($ string-append <> sep <>) (car lst) (cdr lst))))
)
