(library (lyonesse munsch array)

  (export make-array array? array-slice array-data array-type
          make-slice slice? slice-shape slice-stride slice-size slice-offset
          vector->array array->vector array-ref array-iterator
          array-set! array-shape array-data-ptr array-byte-size)
          
  (import (rnrs base (6))
          (rnrs lists (6))
          (rnrs control (6))
          (rnrs records syntactic (6))
          (rnrs io simple (6))

          (only (srfi :1) unfold)

          (lyonesse functional)
          (lyonesse munsch nd-range)

          (lyonesse malloc)

          (only (chezscheme) foreign-ref foreign-set! foreign-sizeof
                             unbox))

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

  #| Array helper routines ================================================= |#
  (define (data-alloc type n)
    (malloc (* n (foreign-sizeof type))))

  (define (data-ref type data pos)
    (foreign-ref type (unbox data) (* (foreign-sizeof type) pos)))

  (define (data-set! type data pos value)
    (foreign-set! type (unbox data) (* (foreign-sizeof type) pos) value))

  (define (size-and-stride shape)
    (let ([cum-prod (fold-right (lambda (x y) (cons (* x (car y)) y))
                                '(1) shape)])
      (values (car cum-prod) (cdr cum-prod))))

  (define (compute-stride shape)
    (let ([cum-prod (fold-right (lambda (x y) (cons (* x (car y)) y))
                                '(1) shape)])
      (cdr cum-prod)))

  #| Working with slices =================================================== |#
  (define-record-type slice
    (fields shape stride size offset)
    (protocol
      (lambda (new)
        (case-lambda
          [(shape)      (let-values ([(size stride) (size-and-stride shape)])
                          (new shape stride size 0))]
          [(shape stride size offset) (new shape stride size offset)]))))

  (define (slice-index slice idx)
    (fold-left (lambda (sum x y) (+ sum (* x y)))
               (slice-offset slice)
               (slice-stride slice)
               idx))

  (define (slice-transpose slice)
    (make-slice (reverse (slice-shape slice))
                (reverse (slice-stride slice))
                (slice-shape slice)
                (slice-offset slice)))

  (define (slice-cut slice axis a b step)
    (let* ([offset (+ (slice-offset slice)
                      (* a (list-ref (slice-stride slice) axis)))]
           [stride (list-mod (slice-stride slice) axis 
                             ($ * step <>))]
           [shape  (list-mod (slice-shape slice) axis 
                             (thunk (div (- b a) (abs step))))]
           [size   (fold-left * 1 shape)])
      (make-slice shape stride size offset)))

  (define (slice-reverse slice axis)
    (let* ([offset (+ (slice-offset slice)
                      (* (list-ref (slice-stride slice) axis)
                         (- (list-ref (slice-shape slice) axis) 1)))]
           [stride (list-mod (slice-stride slice) axis ($ * -1 <>))])
      (make-slice (slice-shape slice) stride (slice-size slice) offset)))


  #| Array routines ======================================================== |#
  (define-record-type array
    (fields type slice data)

    (protocol
      (lambda (new)
        (case-lambda
          [(type shape)      (let ([slice (make-slice shape)])
                               (new type slice (data-alloc type (slice-size slice))))]
          [(type slice data) (new type slice data)]))))

  (define (array-ref a i)
    (data-ref (array-type a) (array-data a)
              (if (nd-range? i) (nd-range-offset i) i)))

  (define (array-set! a offset value)
    (data-set! (array-type a) (array-data a) offset value))

  (define (array-data-ptr a)
    (unbox (array-data a)))

  (define (array-byte-size a)
    (* (foreign-sizeof (array-type a)) (slice-size (array-slice a))))

  (define (array-shape a)
    (slice-shape (array-slice a)))

  (define (array-iterator a)
    (let ([s (array-slice a)])
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

  (define (vector->array type vec)
    (let ([shape (unfold (compose not vector?) vector-length
                         ($ vector-ref <> 0) vec)])
      (when (not (vector-match-shape? vec shape))
        (error 'vector->array "Vector is not uniform." vec))

      (letrec ([a     (make-array type shape)]
               [copy! (lambda (r v)
                        (if (vector? v)
                          (let loop ([r r] [i 0])
                            (if (= i (vector-length v))
                              r
                              (loop (copy! r (vector-ref v i)) (+ i 1))))
                          (begin 
                            (array-set! a r v) (+ r 1))))])
        (copy! 0 vec)
        a)))

  (define (array->vector a)
    (letrec ([vec   (make-vector (car (array-shape a)))]
             [copy! (lambda (r v s)
                      (let loop ([r r] [i 0])
                        (if (= i (car s))
                          r
                          (if (null? (cdr s))
                            (begin
                              (vector-set! v i (array-ref a r))
                              (loop (nd-range-step r) (+ i 1)))
                            (begin
                              (vector-set! v i (make-vector (cadr s)))
                              (loop (copy! r (vector-ref v i) (cdr s)) (+ i 1)))))))])
      (copy! (array-iterator a) vec (array-shape a))
      vec))
)


