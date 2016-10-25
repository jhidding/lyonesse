(library (lyonesse munsch f32array)

  (export make-f32array f32array? f32array-slice f32array-data
          make-slice slice? slice-shape slice-stride slice-size slice-offset
          vector->f32array f32array->vector f32array-ref f32array-iterator
          f32array-set! f32array-shape f32array-bytesize m:f32a)
          
  (import (rnrs base (6))
          (rnrs syntax-case (6))
          (rnrs lists (6))
          (rnrs bytevectors (6))
          (rnrs control (6))
          (rnrs records syntactic (6))
          (rnrs io simple (6))

          (only (srfi :1) unfold)

          (lyonesse functional)
          (lyonesse munsch nd-range)
          (lyonesse munsch slice))

  #| Array helper routines ================================================= |#
  (define bv-sp-ref  bytevector-ieee-single-native-ref)
  (define bv-sp-set! bytevector-ieee-single-native-set!)

  (define (data-alloc n)
    (make-bytevector (* n 4)))

  (define (data-ref data i)
    (bv-sp-ref data (* i 4)))

  (define (data-set! data i value)
    (bv-sp-set! data (* i 4) value))

  #| Array routines ======================================================== |#
  (define-record-type f32array
    (fields slice data)

    (protocol
      (lambda (new)
        (case-lambda
          [(shape)      (let ([slice (make-slice shape)])
                               (new slice (data-alloc (slice-size slice))))]
          [(slice data) (new slice data)]))))

  (define (f32array-ref a i)
    (data-ref (f32array-data a)
              (if (nd-range? i) (nd-range-offset i) i)))

  (define (f32array-set! a offset value)
    (data-set! (f32array-data a) offset value))

  (define (f32array-data-ptr a)
    (f32array-data a))

  (define (f32array-bytesize a)
    (* 4 (slice-size (f32array-slice a))))

  (define (f32array-shape a)
    (slice-shape (f32array-slice a)))

  (define (f32array-iterator a)
    (let ([s (f32array-slice a)])
      (make-nd-range (slice-offset s) (slice-shape s) (slice-stride s))))

  (define (vector-match-shape? vec shape)
    (cond
      [(and (null? shape) (not (vector? vec))) #t]
      [(not (vector? vec)) #f]
      [(= (vector-length vec) (car shape)) 
       (for-all (compose ($ vector-match-shape? <> (cdr shape))
                         ($ vector-ref vec <>))
                (iota (car shape)))]
      [else #f]))

  (define (vector->f32array vec)
    (let ([shape (unfold (compose not vector?) vector-length
                         ($ vector-ref <> 0) vec)])
      (when (not (vector-match-shape? vec shape))
        (error 'vector->f32array "Vector is not uniform." vec))

      (letrec ([a     (make-f32array shape)]
               [copy! (lambda (r v)
                        (if (vector? v)
                          (let loop ([r r] [i 0])
                            (if (= i (vector-length v))
                              r
                              (loop (copy! r (vector-ref v i)) (+ i 1))))
                          (begin 
                            (f32array-set! a r v) (+ r 1))))])
        (copy! 0 vec)
        a)))

  (define (f32array->vector a)
    (letrec ([vec   (make-vector (car (f32array-shape a)))]
             [copy! (lambda (r v s)
                      (let loop ([r r] [i 0])
                        (if (= i (car s))
                          r
                          (if (null? (cdr s))
                            (begin
                              (vector-set! v i (f32array-ref a r))
                              (loop (nd-range-step r) (+ i 1)))
                            (begin
                              (vector-set! v i (make-vector (cadr s)))
                              (loop (copy! r (vector-ref v i) (cdr s)) (+ i 1)))))))])
      (copy! (f32array-iterator a) vec (f32array-shape a))
      vec))

  (define-syntax m:f32a
    (lambda (x)
      (syntax-case x ()
        ;;; 2d array syntax
        [(_ (<row1> ...) ...)
         (with-syntax ([<m> (length #'((<row1> ...) ...))]
                       [<n> (length (car #'((<row1> ...) ...)))])
           #`(let* ([A (make-f32array '(<m> <n>))]
                    [d (f32array-data A)])
               #,@(map (lambda (x n)
                         #`(bv-sp-set! d #,(* 4 n) #,x))
                    (apply append #'((<row1> ...) ...))
                    (iota (* #'<m> #'<n>)))
                A))]
        ;;; 1d array syntax
        [(_ <xs> ...)
         #`(let* ([v (make-f32array '(#,(length #'(<xs> ...))))]
                  [d (f32array-data v)])
             #,@(map (lambda (x n)
                       #`(bv-sp-set! d #,(* 4 n) #,x))
                  #'(<xs> ...) 
                  (iota (length #'(<xs> ...))))
             v)])))
)

