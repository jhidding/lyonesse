(library (lyonesse munsch linear-algebra)

  (export l:vector? make-l:vector l:vector-length l:vector-data
          l:vector-ref l:vector-set! l:v

          l:matrix? make-l:matrix l:matrix-m l:matrix-n l:matrix-data
          l:matrix-ref l:matrix-set! l:m l:eye

          l:* l:T l:vector->row-matrix l:vector->column-matrix

          l:print)

  (import (rnrs base (6))
          (rnrs lists (6))
          (rnrs control (6))
          (rnrs io simple (6))
          (rnrs syntax-case (6))
          (rnrs bytevectors (6))
          (rnrs records syntactic (6))
          (only (chezscheme) trace-define-syntax)
          (srfi :48)

          (lyonesse functional)
          (lyonesse strings))

  (define bv-sp-ref  bytevector-ieee-single-native-ref)
  (define bv-sp-set! bytevector-ieee-single-native-set!)
  (define bv-dp-ref  bytevector-ieee-double-native-ref)
  (define bv-dp-set! bytevector-ieee-double-native-set!)
 
  #| Vector essentials ===================================================== |#
  (define-record-type l:vector
    (fields length data)
    (protocol
      (lambda (new)
        (case-lambda
          [(l) (new l (make-bytevector (* l 4)))]
          [(l data) (new l data)]))))

  (define (l:vector-ref v n)
    (bv-sp-ref (l:vector-data v) (* n 4)))

  (define (l:vector-set! v n x)
    (bv-sp-set! (l:vector-data v) (* n 4) x))

  #| Vector construction =================================================== |#
  (define-syntax l:v
    (lambda (x)
      (syntax-case x ()
        [(_ <xs> ...)
         #`(let* ([v (make-l:vector #,(length #'(<xs> ...)))]
                  [d (l:vector-data v)])
             #,@(map (lambda (x n)
                       #`(bv-sp-set! d #,(* 4 n) #,x))
                  #'(<xs> ...) 
                  (iota (length #'(<xs> ...))))
             v)])))

  #| Vector operations ===================================================== |#
  (define (l:vector-scale a s)
    (let* ([l (l:vector-length a)]
           [b (make-l:vector l)])
      (for-each (lambda (i)
                  (l:vector-set! b i (* (l:vector-ref a i) s)))
                (iota l))
      b))

  (define (l:dot-product a b)
    (apply + (map (lambda (i)
                    (* (l:vector-ref a i) (l:vector-ref b i)))
                  (iota (l:vector-length a)))))

  #| Matrix essentials ===================================================== |#
  (define-record-type l:matrix
    (fields m n data)   ; m rows, n columns
    (protocol
      (lambda (new)
        (case-lambda 
          [(m n) (new m n (make-bytevector (* m n 4)))]
          [(m n data) (new m n data)]))))

  (define (l:matrix-ref A i j)
    (bv-sp-ref (l:matrix-data A) (* (+ j (* i (l:matrix-n A))) 4)))

  (define (l:matrix-set! A i j v)
    (bv-sp-set! (l:matrix-data A) (* (+ j (* i (l:matrix-n A))) 4) v))
 
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
                         #`(bv-sp-set! d #,(* 4 n) #,x))
                    (apply append #'((<row1> ...) ...))
                    (iota (* #'<m> #'<n>)))
                A))])))

  (define (l:eye n)
    (let ([A (make-l:matrix n n)])
      (for-each (lambda (i)
                  (for-each (lambda (j)
                              (l:matrix-set! A i j (if (= i j) 1 0)))
                            (iota n)))
                (iota n))
      A))

  #| Matrix operations ===================================================== |#
  (define (l:matrix-scale A s)
    (let* ([m (l:matrix-m A)] [n (l:matrix-n A)]
           [C (make-l:matrix n m)])
      (for-each (lambda (i)
                  (bv-sp-set!
                    (l:matrix-data C) (* i 4)
                    (bv-sp-ref (l:matrix-data A) (* i 4))))
                (iota (* m n)))
      C))

  (define (l:matrix-transpose A)
    (let* ([m (l:matrix-m A)] [n (l:matrix-n A)]
           [C (make-l:matrix n m)])
      (for-each (lambda (i)
        (for-each (lambda (j)
          (l:matrix-set! C i j (l:matrix-ref A j i)))
          (iota m)))
        (iota n))))

  (define (l:matrix-mul A B)
    (let* ([m (l:matrix-m A)] [n (l:matrix-n A)]
           [p (l:matrix-m B)] [q (l:matrix-n B)]
           [C (make-l:matrix m q)])
      (unless (= n p)
        (error 'l:matrix-mul "Matrix dimensions do not match." A B))
      (for-each (lambda (i)
        (for-each (lambda (j)
          (l:matrix-set! C i j (apply + (map (lambda (k)
                                              (* (l:matrix-ref A i k)
                                                 (l:matrix-ref B k j)))
                                            (iota n)))))
          (iota q)))
        (iota m))
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
      [(and (l:vector? a) (l:vector? b))  (l:dot-product a b)]
      [(and (number? a) (l:vector? b))    (l:vector-scale b a)]
      [(and (l:vector? a) (number? b))    (l:vector-scale a b)]))

  (define (l:* . As)
    (if (null? As)
      (error 'l:* "Need something to multiply" As)
      (fold-right l:mul-2 (car As) (cdr As))))

  #| Output ================================================================ |#
  (define (format-l:matrix A)
    (string-append "[" 
      (string-join "\n "
        (map (lambda (i)
          (string-append "["
            (string-join " "
              (map (lambda (j)
                (format #f "~a" (l:matrix-ref A i j)))
                (iota (l:matrix-n A)))) "]"))
          (iota (l:matrix-m A)))) "]"))

  (define (format-l:vector v)
    (string-append "["
      (string-join " "
        (map (lambda (i)
               (format #f "~a" (l:vector-ref v i)))
             (iota (l:vector-length v)))) "]"))

  (define (l:print A)
    (cond
      [(l:matrix? A) (display (format-l:matrix A))]
      [(l:vector? A) (display (format-l:vector A))]))
)
