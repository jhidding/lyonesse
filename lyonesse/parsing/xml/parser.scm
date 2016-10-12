(library (lyonesse parsing xml parser)
  (export xml:ast-from-port)

  (import (rnrs base (6))
          (rnrs lists (6))
          (rnrs unicode (6))
          (rnrs io ports (6))

          (lyonesse functional)
          (lyonesse parsing ast-builder simple)
          (lyonesse parsing parser))

  #| A parser takes a string and returns a parser and a string.
   |#
  (define (one-of . lst)
    ($ memq <> lst))

  (define (char-identifier? ch)
    (or (char-alphabetic? ch) (char-numeric? ch)
        ((one-of #\- #\_) ch)))

  (define (parse:exit state input)
    (if (procedure? state)
      ((parse:error parse:exit "Unexpected end of file.") 
       state input)
      state))

  (define (parse:error proc msg)
    (lambda (state input)
      (if (procedure? state)
        (error parse:error msg (sb:close-all (state '<here>)) input)
        (error parse:error msg state input))))

  #| We're inside an XML context; until we recieve a closing tag,
   | the rest of this context should be valid (sub)XML.
   |#
  (define-parser (parse:content ch)
    [eof-object?        parse:exit      (sb:close)]
    [char-whitespace?   parse:content   ()]
    [($ char=? #\< <>)  parse:tag       ()]
    [($ char=? #\> <>)     
     (parse:error parse:content "Unexpected character '>'") ()]
    [else               parse:text      (sb:new-string
                                         (sb:add ch))])

  #| We've just opened a tag, we don't know if it is an opening
   | tag or a closing tag.
   |#
  (define-parser (parse:tag ch)
    [eof-object?        (parse:error parse:tag "Unexpected EOF.") ()]
    [($ char=? #\/ <>)  parse:closing-tag
                        (sb:new-symbol (sb:add ch))]
    [($ char=? #\! <>)  parse:!tag ()]
    [($ char=? #\? <>)  parse:?tag ()]
    [char-identifier?   parse:datum
                        (sb:new-list sb:new-symbol (sb:add ch))]
    [else               (parse:error parse:tag "XML syntax error.") ()])


  (define-parser (parse:!tag ch)
    [eof-object?        (parse:error parse:tag "Unexpected EOF.") ()]
    [($ char=? #\- <>)  parse:maybe-comment ()]
    [char-identifier?   parse:datum
                        (sb:new-list sb:new-symbol (sb:add #\!) (sb:add ch))]
    [else               (parse:error parse:!tag "XML syntax error.") ()])

  (define-parser (parse:maybe-comment ch)
    [eof-object?        (parse:error parse:maybe-comment "Unexpected EOF.") ()]
    [($ char=? #\- <>)  parse:comment ()]
    [else               (parse:error parse:maybe-comment "XML syntax error.") ()])
    
  (define (parse:comment state input)
    (let loop ([expecting '(#\- #\- #\>)])
      (cond
        [(null? expecting) 
         (parse:content state input)]
        [(char=? (car expecting) (get-char input))
         (loop (cdr expecting))]
        [else (loop '(#\- #\- #\>))])))

  (define-parser (parse:?tag ch)
    [eof-object?        (parse:error parse:tag "Unexpected EOF.") ()]
    [char-identifier?   parse:?datum
                        (sb:new-list (sb:add '?) sb:new-symbol (sb:add ch))]
    [else               (parse:error parse:?tag "XML syntax error.") ()])

  (define-parser (parse:?datum ch)
    [eof-object?        (parse:error parse:tag "Unexpected EOF.") ()]
    [char-identifier?   parse:?datum
                        ((sb:add ch))]
    [char-whitespace?   parse:attributes
                        (sb:close sb:new-list (sb:add '~))]
    [($ char=? #\? <>)  parse:close-datum
                        (sb:close (sb:add '(~)) (sb:add '?) sb:close)]
    [else               (parse:error parse:tag "XML syntax error.") ()])

  #| It's an opening tag! We parse the name of the tag. If a whitespace
   | follows we parse attributes to this tag, if it closes with '/' we
   | know it is a single datum, if not we enter a new XML context.
   |#
  (define-parser (parse:datum ch)
    [eof-object?        (parse:error parse:tag "Unexpected EOF.") ()]
    [char-identifier?   parse:datum
                        ((sb:add ch))]
    [char-whitespace?   parse:attributes
                        (sb:close sb:new-list (sb:add '~))]
    [($ char=? #\/ <>)  parse:close-datum
                        (sb:close (sb:add '(~)) sb:close)]
    [($ char=? #\> <>)  parse:content
                        (sb:close (sb:add '(~)))]
    [else               (parse:error parse:tag "XML syntax error.") ()])

  #| We've found a single datum tag, the only valid thing to follow is
   | the '>' character.
   |#
  (define-parser (parse:close-datum _)
    [eof-object?        (parse:error parse:close-datum "Unexpected EOF.") ()]
    [($ char=? #\> <>)  parse:content ()]
    [else               (parse:error parse:close-datum "Expected '>'.") ()])

  (define-parser (parse:attributes ch)
    [eof-object?        (parse:error parse:attributes "Unexpected EOF.") ()]
    [char-whitespace?   parse:attributes ()]
    [char-identifier?   parse:attribute-pair
                        (sb:new-list sb:new-symbol (sb:add ch))]
    [($ char=? #\/ <>)  parse:close-datum
                        (sb:close sb:close)]
    [($ char=? #\? <>)  parse:close-datum
                        (sb:close (sb:add '?) sb:close)]
    [($ char=? #\> <>)  parse:content
                        (sb:close)]
    [else               (parse:error parse:attributes "XML syntax error.") ()])
    
  (define-parser (parse:attribute-pair ch) 
    [eof-object?        (parse:error parse:attribute-pair "Unexpected EOF.") ()]
    [char-identifier?   parse:attribute-pair
                        ((sb:add ch))]
    [char-whitespace?   parse:attribute-whitespace
                        (sb:close)]
    [($ char=? #\= <>)  parse:attribute-value
                        (sb:close)]
    [($ char=? #\/ <>)  parse:close-datum
                        (sb:close (sb:add #t) sb:close sb:close)]
    [($ char=? #\> <>)  parse:content
                        (sb:close (sb:add #t) sb:close)]
    [else               (parse:error parse:attribute-pair "XML syntax error.") ()])

  (define-parser (parse:attribute-whitespace ch)
    [eof-object?        (parse:error parse:attribute-whitespace "Unexpected EOF.") ()]
    [char-whitespace?   parse:attribute-whitespace ()]
    [char-identifier?   parse:attribute-pair
                        ((sb:add #t) sb:close sb:new-list sb:new-symbol (sb:add ch))]
    [($ char=? #\= <>)  parse:attribute-value ()]
    [($ char=? #\/ <>)  parse:close-datum
                        ((sb:add #t) sb:close sb:close sb:close)]
    [($ char=? #\> <>)  parse:content
                        ((sb:add #t) sb:close sb:close)]
    [else               (parse:error parse:attribute-whitespace "XML syntax error.") ()])
    
  (define-parser (parse:attribute-value ch)
    [eof-object?        (parse:error parse:attribute-value "Unexpected EOF.") ()]
    [char-whitespace?   parse:attribute-value ()]
    [($ char=? #\/ <>)  parse:close-datum
                        ((sb:add #t) sb:close sb:close sb:close)]
    [($ char=? #\> <>)  parse:content
                        ((sb:add #t) sb:close sb:close)]
    [($ char=? #\" <>)  parse:string
                        (sb:new-string)]
    [else               (parse:error parse:attribute-value "XML syntax error.") ()])
  
  (define-parser (parse:string ch)
    [eof-object?        (parse:error parse:string "Unexpected EOF.") ()]
    [($ char=? #\" <>)  parse:attributes
                        (sb:close sb:close)]
    [($ char=? #\\ <>)  parse:string-esc-char ()]
    [else               parse:string
                        ((sb:add ch))])

  (define-parser (parse:string-esc-char ch)
    [eof-object?        (parse:error parse:string-esc-char "Unexpected EOF.") ()]
    [($ char=? #\" <>)  parse:string ((sb:add #\"))]
    [($ char=? #\n <>)  parse:string ((sb:add #\newline))]
    [else               (parse:error parse:string-esc-char
                                     "Unknown escape character in string.") ()])

  (define-parser (parse:closing-tag ch)
    [eof-object?        (parse:error parse:closing-tag "Unexpected EOF.") ()]
    [char-identifier?   parse:closing-tag ((sb:add ch))]
    [($ char=? #\> <>)  parse:content
                        (sb:close sb:close)]
    [else               (parse:error parse:closing-tag "XML syntax error.") ()])

  (define-parser (parse:text ch)
    [eof-object?        (parse:error parse:text "Unexpected EOF.") ()]
    [($ char=? #\& <>)  parse:text-special-char
                        (($ sb:new (compose xml:special-char list->string) <>))]
    [($ char=? #\< <>)  parse:tag
                        (sb:close)]
    [else               parse:text
                        ((sb:add ch))])

  (define-parser (parse:text-special-char ch)
    [eof-object?        (parse:error parse:text-special-char "Unexpected EOF.") ()]
    [($ char=? #\; <>)  parse:text
                        (sb:close)]
    [else               parse:text-special-char
                        ((sb:add ch))])

  (define (xml:special-char name)
    (let* ([xml:chars '(("lt" . #\<)   ("amp" . #\&) ("gt" . #\>) 
                        ("quot" . #\") ("apos" . #\'))]
           [ch         (cdr (assoc name xml:chars))])
      (if ch ch (error 'xml:special-char "Unknown XML special character." name))))
      
  (define (xml:ast-from-port port)
    (parse:content (sb:start) port))
)

