(library (lyonesse parsing ast-builder simple)
  #|! The simple interface builds an S-expression by composing functions in
   |  continuation passing style. At each point during the process of building
   |  the S-expr the builder state is a function of one argument, placing its
   |  argument in the current position in the S-expr. The returned value may be
   |  another state function or the resulting nested list.
   |
   | > (sb:close (sb:start))
   |    -> ()
   |
   | > (sb:close (sb:add 4 (sb:add 5 (sb:start))))
   |    -> (5 4)
   |
   | > ((compose sb:close 
   |             (sb:add 4)
   |               sb:close
   |               (sb:add 3)
   |               (sb:add 2)
   |               sb:new-list
   |             (sb:add 1)
   |             sb:start))
   |    -> (1 (2 3) 4)
   |
   | > ((compose sb:close sb:close
   |             (sb:add #\o)
   |             (sb:add #\l)
   |             (sb:add #\l)
   |             (sb:add #\e)
   |             (sb:add #\h)
   |             sb:new-string
   |             sb:start))
   |    -> ("hello")
   |
   | > ((compose sb:close sb:close
   |             (sb:add #\a)
   |             (sb:add #\d)
   |             (sb:add #\b)
   |             (sb:add #\m)
   |             (sb:add #\a)
   |             (sb:add #\l)
   |             sb:new-symbol
   |             sb:start))
   |    -> (lambda)
   |#
  (export sb:start sb:new sb:add sb:close sb:close-all
          sb:new-symbol sb:new-string sb:new-list sb:new-datum)

  (import (rnrs base (6))
          (rnrs io ports (6))
          (lyonesse functional))

  (define (sb:start)
    (lambda (x) x))

  (define (sb:new f state)
    (lambda (x)
      (lambda (y)
        (state (cons (f x) y)))))

  (define sb:new-list
    ($ sb:new id <>))

  (define sb:new-string
    ($ sb:new list->string <>))

  (define sb:new-symbol
    ($ sb:new (compose string->symbol list->string) <>))

  (define (sb:close state)
    (state '()))

  (define (sb:close-all state)
    (if (procedure? state)
      (sb:close-all (state '()))
      state))

  (define sb:new-datum
    ($ sb:new (compose get-datum open-string-input-port list->string) <>))

  (define (sb:add item)
    (lambda (state)
      (lambda (x)
        (state (cons item x)))))
)
