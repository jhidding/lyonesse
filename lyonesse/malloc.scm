(library (lyonesse malloc)
  (export malloc)

  (import (rnrs base (6))
          (rnrs control (6))
          (only (chezscheme) foreign-alloc foreign-free
                             box unbox
                             make-guardian
                             collect-request-handler collect))

  (define (do-malloc size)
    (box (foreign-alloc size)))

  (define (do-free x)
    (foreign-free (unbox x)))

  #| Malloc allocates memory, returning a pointer in boxed form.  This box is
   | guarded so that the memory is automatically freed when there are no
   | references to the pointer left.
   |
   | Code from Chez Scheme User Guide (9.4)
   |#

  (define malloc-guardian (make-guardian))

  (define (malloc size)
    (let ([x (do-malloc size)])
      (malloc-guardian x)
      x))

  (collect-request-handler
    (lambda ()
      ; first, invoke the collector
      (collect)
      ; then free any storage that has been dropped
      (let f ()
        (let ([x (malloc-guardian)])
          (when x
            (do-free x)
            (f))))))
)
