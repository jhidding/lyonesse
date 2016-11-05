(library (lyonesse munsch f32array)

  (export make-f32vector f32vector-map f32vector-reduce 
          f32vector-set! f32vector-ref f32v

          make-f32array f32array? f32array-slice f32array-data with-f32array update-f32array
          make-slice slice? slice-shape slice-stride slice-size slice-offset
          f32array-ref f32array-refx f32array-iterator 
          
          f32array-set! f32array-shape f32array-bytesize

          ;;; copying for reuse
          f32array-copy-data
          
          ;;; setters
          f32array-copy! f32array-setx!

          ;;; syntax helpers
          f32a f32array-cut
          
          ;;; formated output
          f32array-format print-f32array)
          
  (import (rnrs base (6))
          (rnrs syntax-case (6))
          (rnrs lists (6))
          (rnrs bytevectors (6))
          (rnrs control (6))
          (rnrs records syntactic (6))
          (rnrs io simple (6))

          (only (srfi :1) unfold)

          (lyonesse functional)
          (lyonesse strings)
          (lyonesse ranges)
          (lyonesse record-with-context)
          (lyonesse munsch nd-range)
          (lyonesse munsch slice))

  #| Array helper routines ================================================= |#
  (define bv-sp-ref  bytevector-ieee-single-native-ref)
  (define bv-sp-set! bytevector-ieee-single-native-set!)

  (define (make-f32vector n)
    (make-bytevector (* n 4)))

  (define (f32vector-ref data i)
    (bv-sp-ref data (* i 4)))

  (define (f32vector-set! data i value)
    (bv-sp-set! data (* i 4) value))

  (define-syntax f32v
    (lambda (x)
      (syntax-case x ()
        [(_ <xs> ...)
         (with-syntax ([<length> (length #'(<xs> ...))])
           #`(let* ([d (make-bytevector (* 4 <length>))])
               #,@(map (lambda (x n)
                       #`(bv-sp-set! d #,(* 4 n) #,x))
                  #'(<xs> ...) 
                  (iota #'<length>))
               d))])))
  
  (define (f32vector-map f . args)
    (let* ([n (bytevector-length (car args))]
           [c (make-bytevector n)])
      (for-range (lambda (i)
        (bv-sp-set! c i (apply f (map ($ bv-sp-ref <> i) args))))
        0 n 4)
      c))

  (define (f32vector-reduce f start a)
    (let ([n (bytevector-length a)])
      (reduce-range (lambda (start i)
        (f start (bv-sp-ref a i)))
        0 0 n 4)))

  #| Array routines ======================================================== |#
  (define-record-with-context f32array
    (fields slice data)

    (protocol
      (lambda (new)
        (case-lambda
          [(shape)      (let ([slice (make-slice shape)])
                               (new slice (make-f32vector (slice-size slice))))]
          [(slice data) (new slice data)]))))

  (define (f32array-dimension a)
    (length (f32array-shape a)))

  (define (f32array-format a)
    (if (= 1 (f32array-dimension a))
      (string-append 
        "[" (string-join 
              " " (map-range (lambda (i)
                               (number->string
                                 (f32array-refx a (i))))
                             (car (f32array-shape a))))
        "]")
      (string-append
        "[" (string-join
              "\n " (map-range (lambda (i)
                                 (f32array-format
                                   (f32array-cut a i)))
                               (car (f32array-shape a))))
         "]")))

  (define (print-f32array a)
    (display (f32array-format a)))

  (define-syntax f32array-cut
    (syntax-rules ()
      [(_ <array> <xs> ...)
       (with-f32array <array>
         (make-f32array (slice-cut slice <xs> ...) data))]))

  (define (f32array-ref a i)
    (f32vector-ref (f32array-data a)
              (if (nd-range? i) (nd-range-offset i) i)))

  (define-syntax f32array-refx
    (syntax-rules ()
      [(_ <array> (<xs> ...))
       (with-f32array <array>
         (f32vector-ref data (slice-index slice (list <xs> ...))))]))

  (define (f32array-set! a offset value)
    (f32vector-set! (f32array-data a) offset value))

  (define (f32array-data-ptr a)
    (f32array-data a))

  (define (f32array-bytesize a)
    (* 4 (slice-size (f32array-slice a))))

  (define (f32array-shape a)
    (slice-shape (f32array-slice a)))

  (define (f32array-stride a)
    (slice-stride (f32array-slice a)))

  (define (f32array-iterator a)
    (let ([s (f32array-slice a)])
      (make-nd-range (slice-offset s) (slice-shape s) (slice-stride s))))

  (define (f32array-copy-data a)
    (let ([data (make-bytevector (f32array-bytesize a))])
      (let loop ([i (f32array-iterator a)]
                 [j 0])
        (unless (nd-range-end? i)
          (f32vector-set! data j 
            (f32vector-ref (f32array-data a) (nd-range-offset i)))
          (loop (nd-range-step i) (inc j))))))

  (define-syntax f32array-setx!
    (syntax-rules ()
      [(_ <array> (<xs> ...) <value>)
       (with-f32array <array>
         (f32vector-set! data (slice-index slice (list <xs> ...)) <value>))]))

  (define (f32array-copy! src tgt)
    (unless ((on equal? f32array-shape) src tgt)
      (error 'f32array-copy! "Array shapes do not match." src tgt))

    (let ([src-data (f32array-data src)]
          [tgt-data (f32array-data tgt)])
      (let loop ([i (f32array-iterator src)]
                 [j (f32array-iterator tgt)])
        (unless (nd-range-end? i)
          (f32vector-set! tgt-data (nd-range-offset j)
            (f32vector-ref src-data (nd-range-offset i)))
          (loop (nd-range-step i) (nd-range-step j))))))

  (define-syntax f32a
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

