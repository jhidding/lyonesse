(import (rnrs (6))
        (prefix (lyonesse testing) test:)
        (lyonesse aux-keyword)
        (lyonesse functional)
        (lyonesse parser)
        (lyonesse sexpr-builder simple))


(define (parse:error state input)
  (error parse:error "Error parsing." state input))

(define (parse:exit state input)
  state)

(define-parser (parse:expr ch)
  [eof-object?      parse:exit   (sb:close-all)]
  [char-whitespace? parse:expr   ()]
  [char-numeric?    parse:number (sb:new-string
                                  (sb:add ch))]
  [($ memq <> '(#\- #\+ #\* #\/))
                    parse:expr   (sb:new-symbol
                                  (sb:add ch)
                                  sb:close)]
  [else parse:error (sb:close-all)])

(define-parser (parse:number ch)
  [eof-object?      parse:exit   (sb:close-all)] 
  [char-numeric?    parse:number ((sb:add ch))]
  [char-whitespace? parse:expr   (sb:close)]
  [else             parse:error  (sb:close-all)])


(test:unit "small parser"
  ([input (open-string-input-port "6 * 7")])

  (test:that "we can read expression"
    (equal? '("6" * "7") (parse:expr (sb:start) input))))

