(library (lyonesse munsch linear-algebra)

  (export l:vector? make-l:vector l:vector-length l:vector-data
          l:vector-ref l:vector-set! l:v

          l:matrix? make-l:matrix l:matrix-m l:matrix-n l:matrix-data
          l:matrix-ref l:matrix-set! l:m l:eye

          l:dot l:* l:T l:vector->row-matrix l:vector->column-matrix
          l:map l:+ l:-

          l:matrix->f32array l:vector->f32array
          f32array->l:matrix f32array->l:vector

          l:print)

  (import (rnrs base (6))
          (rnrs lists (6))
          (rnrs control (6))
          (rnrs io simple (6))
          (rnrs syntax-case (6))
          (rnrs bytevectors (6))
          (rnrs records syntactic (6))
          (srfi :48)

          (lyonesse functional)
          (lyonesse ranges)
          (lyonesse strings)
          (lyonesse record-with-context)
          (lyonesse munsch slice)
          (lyonesse munsch f32array))

  #| Vector essentials ===================================================== |#
  (define-record-with-context l:vector
    (fields length data)
    (protocol
      (lambda (new)
        (case-lambda
          [(l) (new l (make-bytevector (* l 4)))]
          [(l data) (new l data)]))))

  (define (l:vector-ref v n)
    (f32vector-ref (l:vector-data v) n))

  (define (l:vector-set! v n x)
    (f32vector-set! (l:vector-data v) n x))

  #| Vector construction =================================================== |#
  (define-syntax l:v
    (lambda (x)
      (syntax-case x ()
        [(_ <xs> ...)
         #`(let* ([v (make-l:vector #,(length #'(<xs> ...)))]
                  [d (l:vector-data v)])
             #,@(map (lambda (x n)
                       #`(f32vector-set! d #,n #,x))
                  #'(<xs> ...)
                  (iota (length #'(<xs> ...))))
             v)])))

  (define (l:vector->f32array v)
    (with-l:vector v (make-f32array (make-slice (list length)) data)))

  (define (f32array->l:vector a)
    (with-f32array a
      (unless (= 1 (slice-dimension slice))
        (error 'f32array->l:vector "Array should be one-dimensional." a))
      (with-slice slice
        (make-l:vector (car shape)
                       (if (slice-contiguous? slice)
                         data
                         (f32array-copy-data a))))))

  #| Vector operations ===================================================== |#
  (define (l:vector-scale a s)
    (let* ([l (l:vector-length a)]
           [b (make-l:vector l)])
      (for-each (lambda (i)
                  (l:vector-set! b i (* (l:vector-ref a i) s)))
                (iota l))
      b))

  (define (l:dot  a b)
    (apply + (map (lambda (i)
                    (* (l:vector-ref a i) (l:vector-ref b i)))
                  (iota (l:vector-length a)))))

  #| Matrix essentials ===================================================== |#
  (define-record-with-context l:matrix
    (fields m n data)   ; m rows, n columns
    (protocol
      (lambda (new)
        (case-lambda
          [(m n) (new m n (make-bytevector (* m n 4)))]
          [(m n data) (new m n data)]))))

  (define (l:matrix-ref A i j)
    (f32vector-ref (l:matrix-data A) (+ j (* i (l:matrix-n A)))))

  (define (l:matrix-set! A i j v)
    (f32vector-set! (l:matrix-data A) (+ j (* i (l:matrix-n A))) v))

  (define (l:matrix->f32array A)
    (with-l:matrix A
      (make-f32array (make-slice (list m n)) data)))

  (define (f32array->l:matrix a)
    (with-f32array a
      (unless (= 2 (slice-dimension slice))
        (error 'f32array->l:matrix "Array should be two-dimensional." a))
      (with-slice slice
        (make-l:matrix (car shape) (cadr shape)
                       (if (slice-contiguous? slice)
                         data
                         (f32array-copy-data a))))))

  #| Matrix construction =================================================== |#
  (define-syntax l:m
    (lambda (x)
      (syntax-case x ()
        [(_ (<row1> ...) ...)
         (with-syntax ([<m> (length #'((<row1> ...) ...))]
                       [<n> (length (car #'((<row1> ...) ...)))])
           #`(let* ([A (make-l:matrix <m> <n>)]
                    [d (l:matrix-data A)])
               #,@(map (lambda (x n)
                         #`(f32vector-set! d #,n #,x))
                    (apply append #'((<row1> ...) ...))
                    (iota (* #'<m> #'<n>)))
                A))])))

  (define (l:eye n)
    (let ([A (make-l:matrix n n)])
      (for-range (lambda (i)
        (for-range (lambda (j)
          (l:matrix-set! A i j (if (= i j) 1 0)))
          n))
        n)
      A))

  #| Matrix operations ===================================================== |#
  (define (l:matrix-scale A s)
    (with-l:matrix A
      (make-l:matrix n m (f32vector-map ($ * s <>) data))))

  (define (l:matrix-transpose A)
    (let* ([m (l:matrix-m A)] [n (l:matrix-n A)]
           [C (make-l:matrix n m)])
      (for-range (lambda (i)
        (for-range (lambda (j)
          (l:matrix-set! C i j (l:matrix-ref A j i)))
          m))
        n)
      C))

  (define (l:matrix-mul A B)
    (let* ([m (l:matrix-m A)] [n (l:matrix-n A)]
           [p (l:matrix-m B)] [q (l:matrix-n B)]
           [C (make-l:matrix m q)])
      (unless (= n p)
        (error 'l:matrix-mul "Matrix dimensions do not match." A B))
      (for-range (lambda (i)
        (for-range (lambda (j)
          (l:matrix-set! C i j (apply + (map-range (lambda (k)
                                              (* (l:matrix-ref A i k)
                                                 (l:matrix-ref B k j)))
                                            n))))
          q))
        m)
      C))

  #| Matrix <-> Vector ===================================================== |#
  (define (l:vector->row-matrix v)
    (make-l:matrix 1 (l:vector-length v) (l:vector-data v)))

  (define (l:vector->column-matrix v)
    (make-l:matrix (l:vector-length v) 1 (l:vector-data v)))

  (define (l:matrix->vector A)
    (let ([m (l:matrix-m A)] [n (l:matrix-n A)])
      (cond
        [(= 1 m) (make-l:vector n (l:matrix-data A))]
        [(= 1 n) (make-l:vector m (l:matrix-data A))]
        [else (error 'l:matrix->vector "Should be column or row matrix." A)])))

  #| Generic routines ====================================================== |#
  (define (l:T a)
    (cond
      [(l:matrix? a) (l:matrix-transpose a)]
      [(l:vector? a) (l:vector->row-matrix a)]))

  (define (l:mul-2 a b)
    (cond
      [(and (number? a) (number? b))      (* a b)]
      [(and (number? a) (l:matrix? b))    (l:matrix-scale b a)]
      [(and (l:matrix? a) (number? b))    (l:matrix-scale a b)]
      [(and (l:matrix? a) (l:matrix? b))  (l:matrix-mul a b)]
      [(and (l:matrix? a) (l:vector? b))  (l:matrix->vector
                                            (l:matrix-mul a (l:vector->column-matrix b)))]
      [(and (l:vector? a) (l:matrix? b))  (l:matrix->vector
                                            (l:matrix-mul (l:vector->row-matrix a) b))]
      [(and (l:vector? a) (l:vector? b))  (l:dot a b)]
      [(and (number? a) (l:vector? b))    (l:vector-scale b a)]
      [(and (l:vector? a) (number? b))    (l:vector-scale a b)]))

  (define (l:* A . As)
    (fold-right l:mul-2 A As))

  (define (l:map f x . xs)
    (cond
      [(l:matrix? x)
       (with-l:matrix x
         (make-l:matrix m n (apply f32vector-map f
                                   (map l:matrix-data (cons x xs)))))]
      [(l:vector? x)
       (with-l:vector x
         (make-l:vector length (apply f32vector-map f
                                      (map l:vector-data (cons x xs)))))]))

  (define (l:+ . args) (apply l:map + args))
  (define (l:- . args) (apply l:map - args))

  #| Output ================================================================ |#
  (define (l:matrix-format A)
    (f32array-format (l:matrix->f32array A)))

  (define (l:vector-format v)
    (f32array-format (l:vector->f32array v)))

  (define (l:print A)
    (cond
      [(l:matrix? A) (display (l:matrix-format A))]
      [(l:vector? A) (display (l:vector-format A))]))
)
