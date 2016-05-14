#| This code is from R. Kent Dybvig's book 'The Scheme Programming Language',
 | 4th edition, section 12.8.
 |#

(library (oop)
  (export define-object send)

  (import (rnrs (6)))

  (define-syntax define-object
    (syntax-rules ()
      [(_ (<name> . <varlist>)
          ((<var1> <val1>) ...)
          ((<var2> <val2>) ...))
       (define <name>
         (lambda <varlist>
           (let* ([<var1> <val1>] ...)
             (letrec ([<var2> <val2>] ...)
               (lambda (msg . args)
                 (case msg
                   [(<var2>) (apply <var2> args)]
                   ...
                   [else (assertion-violation 
                           '<name> "invalid message" 
                           (cons msg args))]))))))]

      [(_ (<name> . <varlist>) 
          ((<var2> <val2>)...))
       (define-object (<name> . <varlist>)
                      ()
                      ((<var2> <val2>) ...))]))

  (define-syntax send
    (syntax-rules ()
      [(_ <obj> <msg> <arg> ...)
       (<obj> '<msg> <arg> ...)]))
)

