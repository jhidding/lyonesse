(library (lyonesse parsing parser)
  (export define-parser)
  (import (rnrs base (6))
          (rnrs syntax-case (6))
          (only (rnrs io ports (6)) get-char)
          (only (lyonesse functional) pipe))

  (define-syntax define-parser
    (lambda (x)
      (syntax-case x (else)
        [(_ (<name> <ch>)
            [<pred> <next>  (<mutation1> ...)] ...
            [else   <enext> (<emutation1> ...)])
         (syntax 
           (define (<name> state input)
             (let ([<ch> (get-char input)])
               (cond
                 [(<pred> <ch>) 
                  (<next>  (pipe state <mutation1> ...) input)]
                 ...
                 [else          
                  (<enext> (pipe state <emutation1> ...) input)]))))]

        [(_ (<name> <ch>)
            [<pred> <next> (<mutation1> ...)] ...)
         (syntax
           (define (<name> state input)
             (let ([<ch> (get-char input)])
               (cond
                 [(<pred> <ch>) 
                  (<next> (pipe state <mutation1> ...) input)]
                 ...))))])))
)
