(import (rnrs (6))
        (prefix (lyonesse testing) test:)
        (lyonesse functional)
        (lyonesse parsing ast-builder simple))

(test:unit "simple sexpr builder"
  ()

  (test:that "correct values are returned"
    (equal? (pipe (sb:start) sb:close) '())
    (equal? (sb:close ((sb:add 4) ((sb:add 5) (sb:start)))) 
            '(5 4))
    (equal? (pipe (sb:start)
                  (sb:add 1)
                  sb:new-list
                  (sb:add 2)
                  (sb:add 3)
                  sb:close
                  (sb:add 4)
                  sb:close)
            '(1 (2 3) 4))
    (equal? (pipe (sb:start)
                  sb:new-string
                  (sb:add #\h)
                  (sb:add #\e)
                  (sb:add #\l)
                  (sb:add #\l)
                  (sb:add #\o)
                  sb:close sb:close)
            '("hello"))
    (equal? (pipe (sb:start)
                  sb:new-symbol
                  (sb:add #\l)
                  (sb:add #\a)
                  (sb:add #\m)
                  (sb:add #\b)
                  (sb:add #\d)
                  (sb:add #\a)
                  sb:close sb:close)
            '(lambda))
    (= 7.89 (car (pipe (sb:start)
                  sb:new-datum
                  (sb:add #\7)
                  (sb:add #\.)
                  (sb:add #\8)
                  (sb:add #\9)
                  sb:close sb:close)))
))
