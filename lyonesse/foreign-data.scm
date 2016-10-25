(library (lyonesse foreign-data)

  (export deref string->char* get-value-by-ref)
  
  (import (rnrs base (6))
          (rnrs bytevectors (6))

          (lyonesse malloc)
          (lyonesse functional)

          (only (chezscheme) foreign-ref foreign-set! foreign-sizeof unbox
                             foreign-free foreign-alloc))

  (define (new type)
    (malloc (foreign-sizeof type)))

  (define (deref type obj)
    (let ([data (new type)])
      (foreign-set! type (unbox data) 0 (unbox obj))
      data))

  (define (string->char* s)
    (let* ([bv (string->utf8 s)]
           [n  (bytevector-length bv)]
           [fm (malloc (+ n 1))])
      (for-each (lambda (i)
                  (foreign-set! 'unsigned-8 (unbox fm) i 
                                (bytevector-u8-ref bv i)))
                (iota (bytevector-length bv)))
      (foreign-set! 'unsigned-8 (unbox fm) n 0)
      fm))

  (define (get-value-by-ref type proc)
    (let ([x (foreign-alloc (foreign-sizeof type))])
      (proc x)
      (let ([value (foreign-ref type x 0)])
        (foreign-free x)
        value)))
)
