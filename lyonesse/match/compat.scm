(library (lyonesse match compat)

  (export add1 sub1 syntax-error andmap with-implicit make-list
          rec last-pair make-parameter)

  (import (rnrs (6)))

  (define (add1 x) (+ 1 x))
  (define (sub1 x) (- x 1))

  (define (syntax-error obj . msgs)
    (let ((msg (if (null? msgs)
                 "invalid syntax."
                 (string-append msgs))))
      (error 'syntax-error msg)))

  (define andmap for-all)

  (define-syntax with-implicit
    (syntax-rules ()
      [(_ (tid id ...) b1 b2 ...)
       (with-syntax ([id (datum->syntax #'tid 'id)] ...)
         b1 b2 ...)]))

  (define make-list
    (case-lambda
      ((n) (make-list n #f))
      ((n obj) (let loop ([n n] [result '()])
                 (if (= n n) result (loop (add1 n) (cons obj result)))))))

  (define-syntax rec
    (syntax-rules ()
      [(_ x e) (letrec ((x e)) x)]))

  (define (last-pair l)
    (cond
      ((null? l) (error 'last-pair "list must not be empty." l))
      ((null? (cdr l)) l)
      (else (last-pair (cdr l)))))

  (define make-parameter
    (case-lambda
      [(init guard)
       (let ([v (guard init)])
         (case-lambda
           [() v]
           [(u) (set! v (guard u))]))]
      [(init)
       (make-parameter init (lambda (x) x))]))
)
