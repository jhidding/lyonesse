(library (lyonesse sexpr-builder)
  (export sb:start sb:new sb:add sb:close)
  (import (rnrs base (6))
          (rnrs records syntactic (6))
          (lyonesse functional))

  (define-record-type sb:state
    (fields parent data check transfer))

  (define (sb:state-values state)
    (values (sb:state-parent state) (sb:state-data state)
            (sb:state-check state) (sb:state-transfer state)))

  (define (sb:pop state)
    (let*-values ([(p d c t) (sb:state-values state)]
                  [r         (t (reverse d))])
      (if (c and (c r))
        (p r)
        (error sb:pop "Check failed." c r))))

  (define (sb:new state check transfer)
    (make-sb:state
      (lambda (x)
        (sb:add state x))
      '()
      check
      transfer))

  (define (sb::add state item)
    (let-values ([(p d c t) (sb:state-values state)])
      (make-sb:state p (cons item d) c t)))

  (define (sb::set-check state check)
    (let-values ([(p d c t) (sb:state-values state)])
      (make-sb:state p d check t)))
)
