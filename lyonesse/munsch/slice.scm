(library (lyonesse munsch slice)
  (export slice? make-slice slice-shape slice-stride slice-size slice-offset
          slice-index slice-transpose slice-cut slice-reverse
          slice-contiguous? with-slice update-slice

          slice-dimension)

  (import (rnrs base (6))
          (rnrs lists (6))
          (rnrs control (6))
          (rnrs records syntactic (6))
          
          (lyonesse functional)
          (lyonesse record-with-context))

  #|! Returns a new list where the `n`-th element is changed by mapping it
   |  through `proc`.
   |#
  (define (list-mod lst n proc)
    (let loop ([lst    lst]
               [n      n]
               [result '()])
      (cond
        [(null? lst) (reverse result)]
        [(zero? n)   (loop (cdr lst) -1 (cons (proc (car lst)) result))]
        [else        (loop (cdr lst)  (- n 1) (cons (car lst)  result))])))

  (define (list-del lst n)
    (let loop ([lst lst]
               [n   n]
               [result '()])
      (cond
        [(null? lst) (reverse result)]
        [(zero? n)   (loop (cdr lst) (dec n) result)]
        [else        (loop (cdr lst) (dec n) (cons (car lst) result))])))

  (define (size-and-stride shape)
    (let ([cum-prod (fold-right (lambda (x y) (cons (* x (car y)) y))
                                '(1) shape)])
      (values (car cum-prod) (cdr cum-prod))))

  (define (compute-stride shape)
    (let ([cum-prod (fold-right (lambda (x y) (cons (* x (car y)) y))
                                '(1) shape)])
      (cdr cum-prod)))

  #| Working with slices =================================================== |#
  (define-record-with-context slice
    (fields shape stride size offset)
    (protocol
      (lambda (new)
        (case-lambda
          [(shape)      (let-values ([(size stride) (size-and-stride shape)])
                          (new shape stride size 0))]
          [(shape stride size offset) (new shape stride size offset)]))))

  (define (slice-dimension s)
    (length (slice-shape s)))

  (define (slice-index slice idx)
    (with-slice slice
      (apply + offset (map * stride idx))))

  (define (slice-transpose slice)
    (make-slice (reverse (slice-shape slice))
                (reverse (slice-stride slice))
                (slice-shape slice)
                (slice-offset slice)))

  (define (slice-cut-single slice axis a b step)
    (let* ([offset (+ (slice-offset slice)
                      (* a (list-ref (slice-stride slice) axis)))]
           [stride (list-mod (slice-stride slice) axis 
                             ($ * step <>))]
           [shape  (list-mod (slice-shape slice) axis 
                             (thunk (div (- b a) (abs step))))]
           [size   (fold-left * 1 shape)])
      (make-slice shape stride size offset)))

  (define (slice-select-single slice axis x)
    (with-slice slice
      (let* ([offset* (+ offset (* x (list-ref stride axis)))]
             [stride* (list-del stride axis)]
             [shape*  (list-del shape axis)]
             [size*   (fold-right * 1 shape*)])
        (make-slice shape* stride* size* offset*))))

  (define-syntax slice-cut-internal
    (syntax-rules ()
      [(_ <slice> <i>) <slice>]
      [(_ <slice> <i> () <xs> ...)
       (slice-cut-internal <slice> (+ 1 <i>) <xs> ...)]
      [(_ <slice> <i> (<a> <b>) <xs> ...)
       (slice-cut-internal <slice> <i> (<a> <b> 1) <xs> ...)]
      [(_ <slice> <i> (<a> <b> <step>) <xs> ...) 
       (slice-cut-internal
         (slice-cut-single <slice> <i> <a> <b> <step>)
         (+ 1 <i>) <xs> ...)]
      [(_ <slice> <i> <x> <xs> ...)
       (slice-cut-internal
         (slice-select-single <slice> <i> <x>)
         <i> <xs> ...)]))

  (define-syntax slice-cut
    (syntax-rules ()
      [(_ <slice> <xs> ...)
       (slice-cut-internal <slice> 0 <xs> ...)]))

  (define (slice-reverse slice axis)
    (let* ([offset (+ (slice-offset slice)
                      (* (list-ref (slice-stride slice) axis)
                         (- (list-ref (slice-shape slice) axis) 1)))]
           [stride (list-mod (slice-stride slice) axis ($ * -1 <>))])
      (make-slice (slice-shape slice) stride (slice-size slice) offset)))

  (define (slice-contiguous? slice)
    (with-slice slice
      (and (zero? offset) (equal? (compute-stride shape) stride))))
)
