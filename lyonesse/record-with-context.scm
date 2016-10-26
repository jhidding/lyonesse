(library (lyonesse record-with-context)
  (export define-record-with-context)

  (import (rnrs base (6))
          (rnrs syntax-case (6))
          (rnrs records syntactic (6)))

  #| Define a syntactic record type and a context manager.  A R6RS record type
   | comes with accessor functions; for example if we create the record
   | `my-vector` containing the fields `x` and `y` we get accessors `my-vector-x`
   | and `my-vector-y`. Now if we want to use both values in some context we're
   | supoosed to say:
   | 
   | > (define (my-vector-length p) 
   |     (let ([x (my-vector-x p)]
   |           [y (my-vector-y p)])
   |       (sqrt (+ (* x x) (* y y)))))
   |
   | If records become larger, this `let` statement becomes a bit tedious.  We
   | rather do something like:
   |
   | > (define (my-vector-length p)
   |     (with-my-vector p do
   |       (sqrt (+ (* x x) (* y y)))))
   |
   | To make this work, we need to introduce every member of the record type
   | into the local environment using `datum->syntax`. The `(... ...)` syntax
   | is a quoted form of ellipsis for the innner syntax definition.
   |
   | Parts of this code were adapted from Dybvig TSLP ed. 4.
   |#
  (define-syntax define-record-with-context
    (lambda (x)
      ; Define a new symbol, code from TSPL Chapter 8.
      (define gen-id
        (lambda (template-id . args)
          (datum->syntax template-id
            (string->symbol
              (apply string-append
                (map (lambda (x)
                       (if (string? x)
                           x
                           (symbol->string (syntax->datum x))))
                     args))))))

      (syntax-case x (fields)
        [(define-record-with-context <name> (fields <f> ...) <args> ...)
         (with-syntax ([context-manager (gen-id #'<name> "with-" #'<name>)]
                       ; Define the names of member access functions.
                       [(access ...)    (map (lambda (x) 
                                               (gen-id x #'<name> "-" x))
                                             #'(<f> ...))])
           #'(begin
               (define-record-type <name> (fields <f> ...) <args> ...)
               (define-syntax context-manager
                 (lambda (x)
                   (syntax-case x ()
                     [(context-manager <r> <expr> (... ...))
                      (with-syntax ([<f> (datum->syntax #'context-manager '<f>)]
                                    ...)
                        #'(let ([<f> (access <r>)] ...)
                            <expr> (... ...)))])))))])))
)
