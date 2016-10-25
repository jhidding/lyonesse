(library (lyonesse munsch nd-range)

  (export make-nd-range nd-range? 
          nd-range-offset nd-range-index
          nd-range-shape nd-range-carry
          nd-range-values nd-range-end?
          nd-range-step)
          
  (import (rnrs base (6))
          (rnrs lists (6))
          (rnrs control (6))
          (rnrs records syntactic (6))
          
          (lyonesse functional))

  #|! Not equal to, really this should be in the standard.
   |#
  (define /= (compose not =))

  #|! Append two lists, reversing the first one.
   | 
   | > (reverse-append '(3 2 1) (4 5 6))
   |     -> (1 2 3 4 5 6)
   |#
  (define (reverse-append rev lst)
    (if (null? rev)
      lst
      (reverse-append (cdr rev) (cons (car rev) lst))))

  #|! Compute the default strides for a shape.
   |
   | > (compute-stride '(3 4 5))
   |     -> (20 5 1)
   |#
  (define (compute-stride shape)
    (let ([cum-prod (fold-right (lambda (x y) (cons (* x (car y)) y))
                                '(1) shape)])
      (cdr cum-prod)))

  #|! Compute the carry, which is the step between the end of
   |  a row and the begining of the next one.
   |
   | > (compute-carry '(3 76 2))
   |     -> (1 0 0)
   |
   | > (compute-carry '(3 4 5) '(2 12 96))
   |     -> (2 6 48)
   |#
  (define compute-carry
    (case-lambda 
      [(shape) (compute-carry shape (compute-stride shape))]
      [(shape stride)
       (let loop ([carry (list (car (reverse stride)))]
                  [shape (reverse shape)]
                  [stride (reverse stride)])
         (if (null? (cdr stride))
           carry
           (loop (cons (- (cadr stride) (* (car stride) (car shape)))
                       carry)
                 (cdr shape) (cdr stride))))]))

  #|! The nd-range record contains information for doing walks in arrays.
   |
   |  We define three constructors:
   |    `(make-nd-range shape)`: sets the iterator on the beginning of a
   |    contiguous array of the given shape.
   |    `(make-nd-range offset shape stride)`: sets the iterator on the
   |    beginning of a previously sliced array.
   |    `(make-nd-range offset index shape carry)`: the "naked" constructor,
   |    used in each iteration step.
   |
   |  Fields:
   |    `offset`: linear (flat) offset within an array.
   |    `index`: N-d index.
   |    `shape`: size of the array in each dimension.
   |    `carry`: the step between the end on each axis and the beginning of the
   |    next, i.e. when a digit in the index reaches the value given in
   |    `shape`, it is set back to 0, and the `carry` is added as an extra to
   |    the `offset`.
   |#
  (define-record-type nd-range
    (fields offset index shape carry)

    (protocol
      (lambda (new)
        (case-lambda
          [(shape)               (new 0 (map (thunk 0) shape) shape
                                      (compute-carry shape))]
          [(offset shape stride) (new offset (map (thunk 0) shape) shape
                                      (compute-carry shape stride))]
          [(offset index shape carry)
           (new offset index shape carry)]))))

  (define (nd-range-values n)
    (values (nd-range-offset n)
            (nd-range-index n)
            (nd-range-shape n)
            (nd-range-carry n)))

  #|! Checks if an iterator reached the end of the array. The `nd-range-step`
   |  function sets the `offset` field to `#xffffffff` when this happens.
   |#
  (define (nd-range-end? n)
    (= (nd-range-offset n) #xffffffff))

  #|! Move one step in a N-dimensional space. This can be used to do rather
   |  involved iterations on N-dimensional arrays.
   |
   | > (unfold nd-range-end?
   |           nd-range-index
   |           nd-range-step
   |           (make-nd-range '(2 2)))
   |     -> ((0 0) (0 1) (1 0) (1 1))
   |
   | > (unfold nd-range-end?
   |           nd-range-offset
   |           nd-range-step
   |           (make-nd-range 1 '(2 3) '(6 2)))
   |     -> (1 2 5 7 9 11)
   |#
  (define (nd-range-step n)
    (let-values ([(offset index shape* carry*) (nd-range-values n)])
      (let loop ([offset   (+ offset (car (reverse carry*)))]
                 [lo-index (list (+ 1 (car (reverse index))))]
                 [hi-index (cdr (reverse index))]
                 [shape    (reverse shape*)]
                 [carry    (cdr (reverse carry*))])
        (cond
          ; we did not move past the end on this axis
          [(/= (car lo-index) 
               (car shape))   (make-nd-range offset 
                                        (reverse-append hi-index lo-index) 
                                        shape* carry*)]
          ; we moved pased the end of the array, set offset to FFFFFFFF
          [(null? hi-index)   (make-nd-range #xffffffff
                                        (reverse-append hi-index lo-index)
                                        shape* carry*)]
          ; we moved past the end of an axis.
          [else               (loop (+ offset (car carry))
                                   `(,(+ 1 (car hi-index)) 0 . ,(cdr lo-index))
                                    (cdr hi-index) (cdr shape) (cdr carry))]))))
)
